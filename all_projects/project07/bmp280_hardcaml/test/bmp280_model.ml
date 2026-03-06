(* bmp280_model.ml *)open Base

module Bmp280_Model = struct
  type protocol_state = 

    | Idle
    | Address of { mutable bits : int; mutable count : int }
    | Ack_Addr
    | Reg_Pointer of { mutable bits : int; mutable count : int }
    | Ack_Reg
    | Write_Data of { mutable bits : int; mutable count : int }
    | Read_Data of { mutable bits : int; mutable count : int }
    | Send_Ack

  type t = {
    mutable state       : protocol_state;
    mutable last_scl    : int;
    mutable last_sda    : int;
    mutable reg_ptr     : int;
    mutable cycle       : int;
    mutable rw          : int; (* 0 = Write, 1 = Read *)
    mutable device_addr : int;
    regs                : int array; (* 256 registers *)
  }

  let create ~addr = {
    state    = Idle;
    last_scl = 0;
    last_sda = 0;
    reg_ptr  = 0;
    cycle    = 0;
    rw       = 0;
    device_addr = addr; (* Usually 0x76 or 0x77 *)
    regs     = Array.create ~len:256 0;
  }

  let reset_to_defaults t =
    Array.fill t.regs ~pos:0 ~len:256 0;
    t.regs.(0xD0) <- 0x58; (* [Datasheet 4.3.1] Chip ID *)
    t.regs.(0xF3) <- 0x00 (* Status: not measuring, no update *)
    
  let print_state = function
    | Idle -> "Idle"
    | Address _ -> "Address"
    | Ack_Addr -> "Ack_Addr"
    | Reg_Pointer _ -> "Reg_Pointer"
    | Ack_Reg -> "Ack_Reg"
    | Write_Data _ -> "Write_Data"
    | Read_Data _ -> "Read_Data"
    | Send_Ack -> "Send_Ack"  

(* [Datasheet 3.11.3] Official Compensation Formula (Integer version) *)
  let compensate_t t raw_t =
    let dig_t1 = t.regs.(0x88) lor (t.regs.(0x89) lsl 8) in
    let dig_t2 = t.regs.(0x8A) lor (t.regs.(0x8B) lsl 8) in (* Needs signed handling *)
    let var1 = ((raw_t / 16384) - (dig_t1 / 1024)) * dig_t2 in
    (* Simplified for brevity; real version requires 32-bit signed casting *)
    var1 / 4

  let step t ~scl ~sda_in =
    let scl_rising  = (t.last_scl = 0 && scl = 1) in
    let sda_start  = t.last_scl = 1 && t.last_sda = 1 && sda_in = 0 && 
    (match t.state with Ack_Addr | Ack_Reg | Read_Data _ -> false | _ -> true) in
    let sda_stop   = t.last_scl = 1 && t.last_sda = 0 && sda_in = 1 in

    let drive_sda = ref 1 in (* Default High-Z *)

    if sda_start then begin
      Stdio.printf "Start Condition Detected cycle = %d (state: %s, RegPtr: 0x%02x)\n" 
        t.cycle (print_state t.state) t.reg_ptr;
      t.state <- Address { bits = 0; count = 0 }
    end
    else if sda_stop then begin
      Stdio.printf "Stop Condition Detected cycle = %d (state: %s, RegPtr: 0x%02x)\n" 
        t.cycle (print_state t.state) t.reg_ptr;
      t.state <- Idle
    end
    else if scl_rising then begin
      (* Stdio.printf "SCL Rising Edge Detected cycle = %d\n" t.cycle; *)
      (* State machine transitions *)
      match t.state with

      | Address r -> (* Use 'r' to access the record fields *)
          if r.count < 7 then begin
            Stdio.printf "Receiving Address Bit: %d at cycle %d\n" sda_in t.cycle;
            r.bits <- (r.bits lsl 1) lor sda_in; 
            r.count <- r.count + 1
          end else if r.bits = t.device_addr then begin
            Stdio.printf "  Address Received: 0x%02x at cycle %d\n" r.bits t.cycle;
            t.rw <- sda_in; (* LSB indicates R/W *)
            t.reg_ptr <- 0; (* Reset reg pointer on new transaction *)
            t.state <- Ack_Addr;
          end else begin
            Stdio.printf "  Address Mismatch: Received 0x%02x at cycle %d (Expected 0x%02x)\n" r.bits t.cycle (t.device_addr lsl 1);
            t.state <- Idle (* Ignore transaction if address doesn't match *)
          end

      | Ack_Addr ->
          if t.rw = 1 then begin
            Stdio.printf "  ACKed Address, preparing for Read at cycle %d\n" t.cycle;
            t.state <- Read_Data { bits = t.regs.(t.reg_ptr); count = 0 }
          end
          else begin
            Stdio.printf "  ACKed Address, preparing for Write at cycle %d\n" t.cycle;
            t.state <- Reg_Pointer { bits = 0; count = 0 }
          end


      | Reg_Pointer r ->
          if r.count < 8 then begin
            r.bits <- (r.bits lsl 1) lor sda_in; 
            r.count <- r.count + 1
          end else begin
            Stdio.printf "  Register Received: 0x%02x at cycle %d\n" r.bits t.cycle;
            t.reg_ptr <- r.bits; 
            t.state <- Ack_Reg
          end

      | Ack_Reg -> 
          t.state <- Write_Data { bits = 0; count = 0 }
      
      | Write_Data r ->
          if r.count < 8 then begin
            r.bits <- (r.bits lsl 1) lor sda_in;
            r.count <- r.count + 1
          end else begin
            (* Commit the byte to the register array *)
            t.regs.(t.reg_ptr) <- r.bits;
            Stdio.printf " I2C Write: Reg 0x%02x <- 0x%02x\n" t.reg_ptr r.bits;
            (* Auto-increment pointer for burst writes *)
            t.reg_ptr <- (t.reg_ptr + 1) land 0xFF;
            t.state <- Ack_Reg (* Pull SDA low to ACK the data byte *)
          end

      | Read_Data r ->
          if r.count < 7 then r.count <- r.count + 1
          else t.state <- Send_Ack


      | Send_Ack ->
          if sda_in = 0 then begin (* Master ACKs for more data *)
            t.reg_ptr <- (t.reg_ptr + 1) land 0xFF;
            t.state <- Read_Data { bits = t.regs.(t.reg_ptr); count = 0 }
          end else t.state <- Idle (* Master NACKs to end read *)

      | _ -> ()
    end;

    (* Logic to drive SDA low for ACKs *)
    (match t.state with

     | Ack_Addr | Ack_Reg -> drive_sda := 0
     | Read_Data r -> 
        drive_sda := (if t.rw = 1 then (r.bits lsr (7 - r.count)) land 1 else 1);
        Stdio.printf "  Driving SDA for Read: bit %d = %d at cycle %d\n" (7 - r.count) !drive_sda t.cycle
     | _ -> drive_sda := 1);

    t.last_scl <- scl;
    t.last_sda <- sda_in;
    t.cycle <- t.cycle + 1;
    !drive_sda
end
