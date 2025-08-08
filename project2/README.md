Install FTDI USB driver:
------------------------

WSL USB serial adaptor:
-----------------------

We need to install usbipd to connect USB devices to our fedora WSL installation. The latest version can be found here

https://github.com/dorssel/usbipd-win/releases

The latest installer is usbipd-win_5.1.0_x64.msi.

Share Windows USB port with Fedora
----------------------------------

usbipd list

usbipd bind --busid=2-2

Start Fedora 42

usbipd attach --wsl --busid=2-2

In WSL lsusb



Serial Communication with the Tang Nano 20K:
--------------------------------------------

OpenFPGAloader
--------------

openfpgaloader is the tool loading the bitstream into the Tang Nano flash memory from the computer to the board using the USB conector.

    sudo dnf copr enable mobicarte/openFPGALoader
    sudo dnf install openFPGALoader   
