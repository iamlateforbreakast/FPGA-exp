The goal of this project is to interface with the BMP280 pressure and temperature sensor using an I2C interface.

# The BMP280 sensor

The technical spec in (here)[https://www.bosch-sensortec.com/media/boschsensortec/downloads/datasheets/bst-bmp280-ds001.pdf]

After switch on, the sensor initialises in "sleep mode" and needs to transition to "normal mode" to take measurements.

The register adresses containing the mode is:
- 0XF4: mode[0-1] (00 sleep, 10 and 01 forced, 11 normal mode)

The register adresses containing the measurements are:
- 0xF7: pres_msb
- 0xF8: pres_lsb
- 0xF9: pres_xlsb
- 0XFA: temp_msb
- 0xFB: temp_lsb
- 0xFC: temp_xlsb

  It is possible to acquire all measurements in one go by reading 6 bytes at register address 0XF7.
  
# The I2C protocol


# The hardcaml implementation
