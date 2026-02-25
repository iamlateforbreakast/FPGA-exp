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
    regs             : int array; (* 256 registers *)
  }

  let create () = {
    state    = Idle;
    last_scl = 1;
    last_sda = 1;
    reg_ptr  = 0;
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

  let step t ~scl ~sda_in =
    let t.scl_rising_latched  = (t.last_scl = 0 && scl = 1) || (t.last_scl = 1 && scl = 1) in
    let t.scl_falling_latched = (t.last_scl = 1 && scl = 0) || (t.last_scl = 0 && scl = 0) in
    let sda_start  = t.last_scl = 1 && t.last_sda = 1 && sda_in = 0 in
    let sda_stop   = t.last_scl = 1 && t.last_sda = 0 && sda_in = 1 in

    let drive_sda = ref 1 in (* Default High-Z *)

    if sda_start then t.state <- Address { bits = 0; count = 0 }
    else if sda_stop then t.state <- Idle
    else if scl_rising then begin
      match t.state with

      | Address r -> (* Use 'r' to access the record fields *)
          if r.count < 7 then begin
            r.bits <- (r.bits lsl 1) lor sda_in; 
            r.count <- r.count + 1
          end else 
            t.state <- Ack_Addr

      | Ack_Addr -> 
          t.state <- Reg_Pointer { bits = 0; count = 0 }


      | Reg_Pointer r ->
          if r.count < 8 then begin
            r.bits <- (r.bits lsl 1) lor sda_in; 
            r.count <- r.count + 1
          end else begin
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
     | Read_Data { bits; count } -> 
         drive_sda := (bits lsl count) land 0x80
     | _ -> ());

    t.last_scl <- scl;
    t.last_sda <- sda_in;
    !drive_sda
end
