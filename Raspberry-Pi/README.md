## Tartarus for Raspberry Pi

The tartarus platform file for the Raspberry Pi is the file tartarus tartarus_2020v2-2.pl. For more information of usage of Tartarus, refer tartarus user manual in the User_Manuals folder.

These are other utilities available for Raspberry Pi written in SWI Prolog:

### MPU6050

Files: mpu6050.pl ; mpu6050.py

This package allows control over an MPU6050 IMU sensor using a Raspberry Pi through the following steps:

1. Keep the 'mpu6050.py' and the 'mpu6050.pl' files in the same directory as of the 'platform.pl' file. 
2. Consult the 'mpu6050.pl' file into the desired Prolog instantiation.
3. Wire the MPU6050 IMU sensor to the Raspberry Pi.
4. Use the 'mpu6050' predicate to use the sensor.

Please refer the Tartarus User Manual for further details pertaining to the concerned predicate(s).

Note: Before using the sensor, ensure that is detected by the Raspberry Pi. Use the following command in the Raspberry Pi terminal to check the sensor's address (if detected successfully):

i2cdetect -y 1 

If an address is displayed, it denotes that the sensor is sucessfully detected. 


### Servo Motor

Files: servo.pl ; servo.py
This package allows control over a servo motor using a Raspberry Pi through the following steps:

1. Keep the 'servo.py' and the 'servo.pl' files in the same directory as of the 'platform.pl' file. 
2. Consult the 'servo.pl' file into the desired Prolog instantiation.
3. Wire the servo motor to the Raspberry Pi as per the BOARD mode.
4. Use the 'servo' predicate to run the motor.

Please refer the Tartarus User Manual for further details pertaining to the concerned predicate(s).

### LED

Files: led.pl ; led.py

This package allows control over an LED using a Raspberry Pi through the following steps:

1. Keep the 'led.py' and the 'led.pl' files in the same directory as of the 'platform.pl' file. 
2. Consult the 'led.pl' file into the desired Prolog instantiation.
3. Wire the LED to the Raspberry Pi as per the BOARD mode.
4. Use the ledOn/ledOff predicate to control the LED.

Please refer the Tartarus User Manual for further details pertaining to the concerned predicate(s).
