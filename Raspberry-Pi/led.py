import RPi.GPIO as GPIO

GPIO.setmode(GPIO.BOARD)
GPIO.setwarnings(False)
    
def On(Pin):
        Pin = int(Pin)
        GPIO.setup(Pin,GPIO.OUT)
        GPIO.output(Pin,GPIO.HIGH)
        return 'led on'

def Off(Pin):
        Pin = int(Pin)
        GPIO.setup(Pin,GPIO.OUT)
        GPIO.output(Pin,GPIO.LOW)
        return 'led off'
    
