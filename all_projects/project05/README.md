
In YOSYS, type

    read_verilog *.v
    synth_gowin -top uart_test -json uart_test.json -family gw2a

    export PATH=$PATH:~/Tools/nextpnr/build
    nextpnr-himbaechel -r --json uart_test.json --write uart_test_pnr.json --freq 27 --vopt family=GW2A-18C --vopt cst=tangnano20k.cst --device GW2AR-LV18QN88C8/I7
    gowin_pack -d GW2A-18C -o uart_test.fs uart_test_pnr.json
    
    export PATH=$PATH:~/Tools/openFPGALoader/build
    sudo openFPGALoader --ftdi-serial 2025030317 uart_test.fs

Use moserial to see the message sent by the FPGA through /dev/ttyUSB1

```
# Set parameters (baud rate 115200, raw input, etc.)
sudo stty -F /dev/ttyUSB1 115200 raw -echo
# Dump the data
sudo cat /dev/ttyUSB1 > output.log
```

or

```
sudo minicom -D /dev/ttyUSB1 -b 115200 -C output.log
```
