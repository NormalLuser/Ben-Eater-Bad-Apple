import argparse
import os

#Fifty1Ford/NormalLuser Bad Apple Run Length Encoding (RLE) routine.
#Modified version of: from :


def main():
    MAX_BYTE_COUNT = 255
    output_file = "BadAppleOUTDIFFEdit.bin"
    RLEcount=0
    prev_byte=64
    mybyte=b'1'
    print('Output file:', output_file)
    SkipByte=64
    if os.path.isfile(output_file):
        os.remove(output_file)
    FileCounter=30
    match=0
    nomatch=0
    RLEcount=0
    Previousbyte1=0x40
    Currentbyte1=0x40
    ScreenCounter =0
    mybyte = 0x40

    with open(output_file, 'ba') as g:
        while FileCounter < 6571: #The python script at 30fps outputs 6,571 frames.
                                  # 219 second, 3.65 min orgional video
            PreviousFrame = open("BadApple" + str(FileCounter) + ".txt", "rb")#previous frame
            CurrentFrame = open("BadApple" + str((FileCounter + 1)) + ".txt", "rb")#current frame

            Previousbyte1 = PreviousFrame.read(1)
            Currentbyte1 = CurrentFrame.read(1)
            ScreenCounter=1 #we read First byte of frame

            while Previousbyte1!=b'':
                if Previousbyte1==Currentbyte1: #Match
                   mybyte = SkipByte #Skip value
                else: # No Match
                    mybyte = int.from_bytes(Currentbyte1, "little")

                if ScreenCounter>100: #Skip 28 bytes every 100
                    if ScreenCounter>128:#Back in on-screen area
                        ScreenCounter=1#reset and don't change pixel color
                    else:
                        mybyte = SkipByte #make it a skip pixel no matter what

                #RLE ROUTINE
                if RLEcount==MAX_BYTE_COUNT:
                    #write_byte(output_file, RLEcount, prev_byte)
                    g.write(bytes([RLEcount]))
                    g.write(bytes([prev_byte]))
                    prev_byte = mybyte
                    RLEcount = 0

                if mybyte != prev_byte:
                    #write_byte(output_file, RLEcount, prev_byte)
                    g.write(bytes([RLEcount]))
                    g.write(bytes([prev_byte]))
                    prev_byte = mybyte
                    RLEcount = 1
                else:
                    RLEcount += 1

                #READ BYTES
                Previousbyte1  = PreviousFrame.read(1)
                Currentbyte1  = CurrentFrame.read(1)
                ScreenCounter += 1
            #End While Previousbyte1!=b''

            FileCounter += 1            #g.write(bytes([RLEcount]))

        #End Files

    #End output

main()

