open Base

module Bmp280_Model = struct
  type protocol_state = 

    | Idle
    | Address of { mutable bits : int; mutable count : int }
    | Ack_Addr

    | Reg_Pointer of { mutable bits : int; mutable count : int }
    | Ack_Reg
    | Read_Data of { mutable bits : int; mutable count : int }

    | Send_Ack

  type t = {
    mutable state    : protocol_state;
    mutable last_scl : int;
    mutable last_sda : int;
    mutable reg_ptr  : int;
    mutable cycle    : int;
    mutable rw       : int; (* 0 = Write, 1 = Read *)
    regs             : int array; (* 256 registers *)
  }

  let create () = {
    state    = Idle;
    last_scl = 0;
    last_sda = 0;
    reg_ptr  = 0;
    cycle    = 0;
    rw       = 0;
    regs     = Array.create ~len:256 0;
  }

  (* Pre-load Calibration & ID *)
  let setup_default_values t =
    t.regs.(0xD0) <- 0x58; (* Chip ID *)
    t.regs.(0x88) <- 0xA1; (* Example Calibration dig_T1 *)
    ()

  (* The "Gold" Compensation Logic (Temperature only for brevity) *)
  let compensate_t t raw_t =
    let dig_t1 = t.regs.(0x88) lor (t.regs.(0x89) lsl 8) in
    (* Official Bosch 64-bit logic goes here *)
    (raw_t / 16384) * dig_t1 (* Simplified placeholder *)
  let print_state s =
    match s with
    | Idle -> "Idle"
    | Address _ -> "Address"
    | Ack_Addr -> "Ack_Addr"
    | Reg_Pointer _ -> "Reg_Pointer"
    | Ack_Reg -> "Ack_Reg"
    | Read_Data _ -> "Read_Data"
    | Send_Ack -> "Send_Ack"
  let step t ~scl ~sda_in =
    let scl_rising  = (t.last_scl = 0 && scl = 1) in
    let sda_start  = t.last_scl = 1 && t.last_sda = 1 && sda_in = 0 in
    let sda_stop   = t.last_scl = 1 && t.last_sda = 0 && sda_in = 1 in

    let drive_sda = ref 1 in (* Default High-Z *)

    if sda_start then begin
      Stdio.printf "Start Condition Detected cycle = %d\n" t.cycle;
      t.state <- Address { bits = 0; count = 0 }
    end
    else if sda_stop then begin
      Stdio.printf "Stop Condition Detected cycle = %d\n" t.cycle;
      t.state <- Idle
    end
    else if scl_rising then begin
      Stdio.printf "SCL Rising Edge Detected cycle = %d\n" t.cycle;
      (* State machine transitions *)
      match t.state with

      | Address r -> (* Use 'r' to access the record fields *)
          if r.count < 8 then begin
            (* Stdio.printf "Receiving Address Bit: %d at cycle %d\n" sda_in t.cycle; *)
            r.bits <- (r.bits lsl 1) lor sda_in; 
            r.count <- r.count + 1
          end else begin
            Stdio.printf "Address Received: 0x%02x at cycle %d\n" r.bits t.cycle;
            t.rw <- r.bits land 1; (* LSB indicates R/W *)
            t.reg_ptr <- 0; (* Reset reg pointer on new transaction *)
            t.state <- Ack_Addr;
          end

      | Ack_Addr ->
          if t.rw = 0 then t.state <- Reg_Pointer { bits = 0; count = 0 }
          else t.state <- Read_Data { bits = t.regs.(t.reg_ptr); count = 0 }


      | Reg_Pointer r ->
          if r.count < 8 then begin
            r.bits <- (r.bits lsl 1) lor sda_in; 
            r.count <- r.count + 1
          end else begin
            Stdio.printf "Register Received: 0x%02x at cycle %d\n" r.bits t.cycle;
            t.reg_ptr <- r.bits; 
            t.state <- Ack_Reg
          end

      | Ack_Reg -> 
          t.state <- Read_Data { bits = t.regs.(t.reg_ptr); count = 0 }
      | _ -> ()
    end;

    (* Logic to drive SDA low for ACKs *)
    (match t.state with

     | Ack_Addr | Ack_Reg -> drive_sda := 0
     | Address _ | Reg_Pointer _ -> () (* Slave listens, doesn't drive *)
     | Read_Data { bits; count } -> 
         drive_sda := (bits lsl count) land 0x80
     | _ -> ());

    t.last_scl <- scl;
    t.last_sda <- sda_in;
    t.cycle <- t.cycle + 1;
    !drive_sda
end
