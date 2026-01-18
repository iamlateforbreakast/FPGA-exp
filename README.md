# FPGA-exp
A few experiments targetting the Sipeed Tang Nano 20K. The general aim would be to form a general view about the FPGA development process regarding complexity and development time as well as testing efficiency.

Setting up the Sipeed toolchain
-------------------------------

The Sipeed Tang Nano 20K is descrbed here: https://wiki.sipeed.com/hardware/en/tang/tang-nano-20k/nano-20k.html

Projects
--------

| P  | Desc. | Status | Test |
| -- | ------------------------------------------ | ------- | ------ |
| 1 | [Evaluate the Sipeed proprietary toolchain](https://github.com/iamlateforbreakast/FPGA-exp/tree/main/all_projects/project01) | Not started | N/A |
| 2 | [Evaluate the open source toolchain](https://github.com/iamlateforbreakast/FPGA-exp/tree/main/all_projects/project02) | Not started | N/A |
| 3 | [Evaluate the Hardcaml toolchain](https://github.com/iamlateforbreakast/FPGA-exp/tree/main/all_projects/project03) | Started | TBD |
| 4 | [Use GPIO to drive LEDS](https://github.com/iamlateforbreakast/FPGA-exp/tree/main/all_projects/project04) | Working | [![Project04](https://github.com/iamlateforbreakast/FPGA-exp/actions/workflows/project04.yml/badge.svg)](https://github.com/iamlateforbreakast/FPGA-exp/actions/workflows/project04.yml) |
| 5 | [UART project](https://github.com/iamlateforbreakast/FPGA-exp/tree/main/all_projects/project05) | In progress| [![Project05](https://github.com/iamlateforbreakast/FPGA-exp/actions/workflows/project05.yml/badge.svg)](https://github.com/iamlateforbreakast/FPGA-exp/actions/workflows/project05.yml) |
| 6 | [HDMI project](https://github.com/iamlateforbreakast/FPGA-exp/tree/main/all_projects/project06) | Started | [![Project06](https://github.com/iamlateforbreakast/FPGA-exp/actions/workflows/project06.yml/badge.svg)](https://github.com/iamlateforbreakast/FPGA-exp/actions/workflows/project06.yml) |
| 7 | [I2C project](https://github.com/iamlateforbreakast/FPGA-exp/tree/main/all_projects/project07) | Not started | TBD |
| 8 | [SPI project](https://github.com/iamlateforbreakast/FPGA-exp/tree/main/all_projects/project08) | Started | [![Project08](https://github.com/iamlateforbreakast/FPGA-exp/actions/workflows/project8.yml/badge.svg)](https://github.com/iamlateforbreakast/FPGA-exp/actions/workflows/project8.yml) |
| 9 | [SDCard project](https://github.com/iamlateforbreakast/FPGA-exp/tree/main/all_projects/project09) | Not started | TBD |
| 10 | [USB project](https://github.com/iamlateforbreakast/FPGA-exp/tree/main/all_projects/project10) | Not started | TBD |
| 11 | [ADC/DAC project](https://github.com/iamlateforbreakast/FPGA-exp/tree/main/all_projects/project11) | Not started | TBD |
| 12 | [Advanced project](https://github.com/iamlateforbreakast/FPGA-exp/tree/main/all_projects/project12) | Not started | TBD |

Interesting Resources
---------------------

Using open source toolchain on Sipeed Tang Nano 9K.

https://learn.lushaylabs.com/getting-setup-with-the-tang-nano-9k/

Using Raspberry Pi 5 and open source toolchain.

https://www.weigu.lu/other_projects/fpga/fpga_tang_nano_9k/index.html

Interesting H/W blog and FPGA experiements.

https://specbranch.com/

Quick and simple tutorial to flash a blink project on Sipeed Tang nanon 9K using open source toolchain.

https://blog.peramid.es/posts/2024-10-19-fpga.html

Brilliant blog with interesting FPGA projects and installing open source chain on docker on Sipeed Tang Nano 20K.

https://www.mikekohn.net/micro/tang_nano_dev.php

Detailled blog implementing a NERV32 on Nano tang 20k

https://www.jpfau.org/series/oss-risc-v-development-on-tang-nano/

List of project tagged #GOWIN on github

https://github.com/topics/gowin?o=desc&s=forks

Gowin PLL calculator

https://juj.github.io/gowin_fpga_code_generators/pll_calculator.html

