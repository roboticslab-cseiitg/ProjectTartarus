import RPi.GPIO as GPIO


def init(Pin,Fr,Start):
        GPIO.setwarnings(False)
        Pin=int(Pin)
        Fr=int(Fr)
        Start=float(Start)
        GPIO.setmode(GPIO.BOARD)
        GPIO.setup(Pin,GPIO.OUT)
        init.p = GPIO.PWM(Pin,Fr)
        init.p.start(Start)
        return 'moved'
       
        
