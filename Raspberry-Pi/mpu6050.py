from math import *
import smbus			


PWR_MGMT_1   = 0x6B
SMPLRT_DIV   = 0x19
CONFIG       = 0x1A
GYRO_CONFIG  = 0x1B
INT_ENABLE   = 0x38
ACCEL_XOUT_H = 0x3B
ACCEL_YOUT_H = 0x3D
ACCEL_ZOUT_H = 0x3F
GYRO_XOUT_H  = 0x43
GYRO_YOUT_H  = 0x45
GYRO_ZOUT_H  = 0x47
TEMP_OUT = 0x41
bus = smbus.SMBus(1) 	
Device_Address=0	
for device in range(128):

      try:
         bus.read_byte(device)
         Device_Address=device
      except: 
         pass

        
    
def MPU_Init(Device_Address):
	bus.write_byte_data(Device_Address, SMPLRT_DIV, 7)
	
	bus.write_byte_data(Device_Address, PWR_MGMT_1, 1)
	
	bus.write_byte_data(Device_Address, CONFIG, 0)
	
	bus.write_byte_data(Device_Address, GYRO_CONFIG, 24)
	
	bus.write_byte_data(Device_Address, INT_ENABLE, 1)

def read_raw_data(Device_Address,addr):
	high = bus.read_byte_data(Device_Address, addr)
        low = bus.read_byte_data(Device_Address, addr+1)
        value = ((high << 8) | low)
        
        if(value > 32768):
                value = value - 65536
        return value

def all():
        MPU_Init(Device_Address)
        acc_x = read_raw_data(Device_Address,ACCEL_XOUT_H)
        acc_y = read_raw_data(Device_Address,ACCEL_YOUT_H)
        acc_z = read_raw_data(Device_Address,ACCEL_ZOUT_H)
        raw_temp = read_raw_data(Device_Address,TEMP_OUT)
        Ax = acc_x/16384.0
        Ay = acc_y/16384.0
        Az = acc_z/16384.0
        temp = (raw_temp/340) + 36.53
        vals = []
        roll=(atan2(Ay,Az)*180.0)/pi
        pitch=(atan2(Ax,Ay)*180.0)/pi
        yaw=(atan2(Ax,Az)*180.0)/pi
        vals.append(roll)
        vals.append(pitch)
        vals.append(yaw)
        vals.append(temp)
        return vals
    
def roll():
        MPU_Init(Device_Address)
        acc_y = read_raw_data(Device_Address,ACCEL_YOUT_H)
        acc_z = read_raw_data(Device_Address,ACCEL_ZOUT_H)
        Ay = acc_y/16384.0
        Az = acc_z/16384.0
        roll=(atan2(Ay,Az)*180.0)/pi
        return roll
    
def pitch():
        MPU_Init(Device_Address)
        acc_x = read_raw_data(Device_Address,ACCEL_XOUT_H)
        acc_y = read_raw_data(Device_Address,ACCEL_YOUT_H)
        Ax = acc_x/16384.0
        Ay = acc_y/16384.0
        pitch=(atan2(Ax,Ay)*180.0)/pi
        return pitch

def yaw():
        MPU_Init(Device_Address)
        acc_x = read_raw_data(Device_Address,ACCEL_XOUT_H)
        acc_z = read_raw_data(Device_Address,ACCEL_ZOUT_H)
        Ax = acc_x/16384.0
        Az = acc_z/16384.0
        yaw=(atan2(Ax,Az)*180.0)/pi
        return yaw
    
def temp():
        MPU_Init(Device_Address)
        raw_temp = read_raw_data(Device_Address,TEMP_OUT)
        temp = (raw_temp/340) + 36.53
        return temp
    
def reg():
        MPU_Init(Device_Address)
        Ax1 = bus.read_byte_data(Device_Address, ACCEL_XOUT_H)
        Ax2 = bus.read_byte_data(Device_Address, ACCEL_XOUT_H+1)
        Ay1 = bus.read_byte_data(Device_Address, ACCEL_YOUT_H)
        Ay2 = bus.read_byte_data(Device_Address, ACCEL_YOUT_H+1)
        Az1 = bus.read_byte_data(Device_Address, ACCEL_ZOUT_H)
        Az2 = bus.read_byte_data(Device_Address, ACCEL_ZOUT_H+1)
        T1 = bus.read_byte_data(Device_Address, TEMP_OUT)
        T2 = bus.read_byte_data(Device_Address, TEMP_OUT+1)
        rval = []
        rval.append(Ax1)
        rval.append(Ax2)
        rval.append(Ay1)
        rval.append(Ay2)
        rval.append(Az1)
        rval.append(Az2)
        rval.append(T1)
        rval.append(T2)
        return rval
    
