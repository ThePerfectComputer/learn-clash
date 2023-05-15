import serial
import time
import numpy as np

def main():
    # Configure the serial connection.
    # Replace '/dev/ttyUSB0' with the appropriate
    # device path for your serial device.
    ser = serial.Serial('/dev/tty.usbserial-K00027',
                        baudrate=9600,
                        bytesize=8,
                        parity='N',
                        stopbits=2,
                        timeout=1)

    # Send data
    data_to_send = np.array([0, 1, 0], dtype=np.uint8).tobytes()
    ser.write(data_to_send)

    # Allow some time for the data to be transmitted and received
    time.sleep(.1)

    # Receive data
    received_data = ser.read(ser.in_waiting)
    array = np.frombuffer(received_data, dtype=np.uint8)
    print(array)
    # print("Received data:",
    #       ' '.join([f'{byte:02X}' for byte in received_data]))

    # Close the serial connection
    ser.close()

if __name__ == '__main__':
    main()
