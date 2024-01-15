import argparse
import os

# Fifty1Ford/NormalLuser Bad Apple One Byte Diff Run Length Encoding (RLE) routine.
# Started as a modified version of: nes-rle-decompress from :https://github.com/samcan/nes-rle-decompress
# This is a working 1 byte encoder.
# 30 Frames a Second original video extract.
# 53 Frames a Second on Average without vsync
# The original uncompressed data is:  52,336 KB
# This encoder packs that down to:     2,568 KB
# More than a 20 to 1 compression ratio
# 95% space savings! Pretty good for something so simple
#With 1 byte the encoding is:
#  If bit 8 is 1 then the next 7 bits are used to encode a skip of up to 127 pixels
#  If bit 8 is 0 then bit 7 is checked:
#    If bit 7 is 1 then the next 6 bits are used to encode a repeat of up to 63 pixels
#    If bit 7 is 0 then the next 6 bits are used to encode a 'TriPixel'
#      A TriPixel is 3 pixels of 4 colors (black, dark grey, light grey, white)
#      the table below is the lookup for this.

def main():
    PixelList = [(0, 0, 0, 0), (0, 0, 21, 1), (0, 21, 0, 2), (21, 0, 0, 3), (0, 0, 42, 4),
                 (0, 42, 0, 5), (42, 0, 0, 6), (0, 0, 63, 7), (0, 63, 0, 8), (63, 0, 0, 9),
                 (0, 21, 21, 10), (21, 0, 21, 11), (21, 21, 0, 12), (0, 21, 42, 13),
                 (0, 42, 21, 14), (21, 0, 42, 15), (21, 42, 0, 16), (42, 0, 21, 17),
                 (42, 21, 0, 18), (0, 21, 63, 19), (0, 63, 21, 20), (21, 0, 63, 21),
                 (21, 63, 0, 22), (63, 0, 21, 23), (63, 21, 0, 24), (0, 42, 42, 25),
                 (42, 0, 42, 26), (42, 42, 0, 27), (0, 42, 63, 28), (0, 63, 42, 29),
                 (42, 0, 63, 30), (42, 63, 0, 31), (63, 0, 42, 32), (63, 42, 0, 33),
                 (0, 63, 63, 34), (63, 0, 63, 35), (63, 63, 0, 36), (21, 21, 21, 37),
                 (21, 21, 42, 38), (21, 42, 21, 39), (42, 21, 21, 40), (21, 21, 63, 41),
                 (21, 63, 21, 42), (63, 21, 21, 43), (21, 42, 42, 44), (42, 21, 42, 45),
                 (42, 42, 21, 46), (21, 42, 63, 47), (21, 63, 42, 48), (42, 21, 63, 49),
                 (42, 63, 21, 50), (63, 21, 42, 51), (63, 42, 21, 52), (21, 63, 63, 53),
                 (63, 21, 63, 54), (63, 63, 21, 55), (42, 42, 42, 56), (42, 42, 63, 57),
                 (42, 63, 42, 58), (63, 42, 42, 59), (42, 63, 63, 60), (63, 42, 63, 61),
                 (63, 63, 42, 62), (63, 63, 63, 63)]

    #The above array is duplicated on the 6502 side as 3 arrays
    # This is 192 bytes accessed with:
    # LDA .Array1,x
    #..
    # LDA .Array2,x
    #..
    # LDA .Array3,x
    #make sure to keep this in the same page and it will be the same speed as a zero page table in ROM or normal RAM

    # .Array1:;
    # .byte  0, 0, 0, 21, 0, 0, 42, 0, 0, 63, 0, 21, 21, 0, 0, 21, 21, 42, 42, 0, 0, 21, 21, 63, 63, 0, 42, 42, 0, 0, 42, 42, 63, 63, 0, 63, 63, 21, 21, 21, 42, 21, 21, 63, 21, 42, 42, 21, 21, 42, 42, 63, 63, 21, 63, 63, 42, 42, 42, 63, 42, 63, 63, 63
    # .Array2:;
    # .byte 0, 0, 21, 0, 0, 42, 0, 0, 63, 0, 21, 0, 21, 21, 42, 0, 42, 0, 21, 21, 63, 0, 63, 0, 21, 42, 0, 42, 42, 63, 0, 63, 0, 42, 63, 0, 63, 21, 21, 42, 21, 21, 63, 21, 42, 21, 42, 42, 63, 21, 63, 21, 42, 63, 21, 63, 42, 42, 63, 42, 63, 42, 63, 63
    # .Array3:;
    # .byte 0, 21, 0, 0, 42, 0, 0, 63, 0, 0, 21, 21, 0, 42, 21, 42, 0, 21, 0, 63, 21, 63, 0, 21, 0, 42, 42, 0, 63, 42, 63, 0, 42, 0, 63, 63, 0, 21, 42, 21, 21, 63, 21, 21, 42, 42, 21, 63, 42, 63, 21, 42, 21, 63, 63, 21, 42, 63, 42, 42, 63, 63, 42, 63

    MAX_BYTE_COUNT = 61 #63 #127 #255 #RLE length
    MAX_SKIP_COUNT = 127
    LastColor=253
    LastTriMatch=False

    output_file = "BadAppleNew1Byte7Big.bin"
    #output_file = "SteamboatWillieExtraBW1byte7.bin"
    RLEcount=0
    prev_byte=128
    mybyte=b'1'
    print('Output file:', output_file)
    SkipByte=128
    RLEByte=64
    if os.path.isfile(output_file):
        os.remove(output_file)
    FileCounter=29



    match=0
    nomatch=0
    RLEcount=0
    Previousbyte1=0x40
    Currentbyte1=0x40
    ScreenCounter =0
    mybyte = 0x40


    #vars for Lookup for TriBytes
    #This version of the encoder will just assume valid B/W/G/G colors.
    #I can make this fancy later (or just use older encoder for color content)

    StartTri=False #True #??First bytes of first image should be a Tri??
    TriCount=0
    TriMatch=False
    TriNumber=0
    TriByte=[0,0,0]
    TriMatchCount =0
    LastTriMatchCountOut=0
    VChangeLimit=500
    VChangeLimit2 = 200
    VChangeCounter=0
    TBeep=255
    TVsync=1
    TVsync2 = 2
    VchangeSkip=0
    VchangeOne=0
    VchangeTwo=0

    cntSkip=0
    cntTri=0

    FirstSkip=1

    FileRepeate=0


    ScreenCounter = 1  # we read First byte of frame, We know this is first position in stream
    with open(output_file, 'ba') as g:

        while FileRepeate<1: #Change this to make a bigger 'looped' file
            FileRepeate+=1
            FileCounter = 29

            while FileCounter < 6570: #The python script at 30fps outputs 6,571 frames. 13257: for steamboat
                # 219 second, 3.65 min orgional video
                PreviousFrame = open("BadApple" + str(FileCounter) + ".txt", "rb")#previous frame
                CurrentFrame = open("BadApple" + str((FileCounter + 1)) + ".txt", "rb")#current frame
                Previousbyte1 = PreviousFrame.read(1)
                Currentbyte1 = CurrentFrame.read(1)

                #Below is hack to adjust centering
                if FirstSkip==1:
                    FirstSkip=0
                    Previousbyte1 = PreviousFrame.read(1)
                    Currentbyte1 = CurrentFrame.read(1)
                    Previousbyte1 = PreviousFrame.read(1)
                    Currentbyte1 = CurrentFrame.read(1)
                    Previousbyte1 = PreviousFrame.read(1)
                    Currentbyte1 = CurrentFrame.read(1)
                    Previousbyte1 = PreviousFrame.read(1)
                    Currentbyte1 = CurrentFrame.read(1)
                    Previousbyte1 = PreviousFrame.read(1)
                    Currentbyte1 = CurrentFrame.read(1)
                    Previousbyte1 = PreviousFrame.read(1)
                    Currentbyte1 = CurrentFrame.read(1)
                    Previousbyte1 = PreviousFrame.read(1)
                    Currentbyte1 = CurrentFrame.read(1)






                while Previousbyte1!=b'':
                    if StartTri==True:
                        TriByte[TriCount]=prev_byte
                        TriCount+=1
                        mybyte = int.from_bytes(Currentbyte1, "little")
                        prev_byte=mybyte
                        VChangeCounter+=1
                        if TriCount==3:
                            StartTri = False
                            TriCount=0
                            CheckBytes = TriByte[0], TriByte[1], TriByte[2]

                            #Lets hack in a test to see if all of the tripixel matches the
                            #last color used? This way we can just use a RLE and skip the Tripixel
                            #try to save a few K and speed up decode.
                            if TriByte[0] == LastColor and TriByte[1] == LastColor and TriByte[2] == LastColor:
                                LastTriMatch = True
                                TriMatchCount +=1
                                ##print  ("LastTriMatch", TriMatchCount)
                            else:
                                LastTriMatch = False

                            for y in range(len(PixelList)):
                                ByteA = PixelList[y][0]
                                ByteB = PixelList[y][1]
                                ByteC = PixelList[y][2]
                                ByteD = PixelList[y][3]
                                MYPixelListCheck = ByteA, ByteB, ByteC

                                if CheckBytes == MYPixelListCheck:
                                    TriMatch = True
                                    TriNumber = ByteD #$+ 65  # PixelList[y][3]
                                    RLEcount = 1#count = 0  # not sure if 0 or 1??
                                    LastColor = TriByte[2]
                                    mybyte = TriByte[2]
                                    prev_byte = mybyte
                                    break;
                        mybyte = int.from_bytes(Currentbyte1, "little")

                    if StartTri==False:#Not the start of a Tri
                        mybyte = int.from_bytes(Currentbyte1, "little")

                        if Previousbyte1==Currentbyte1: #Match
                           mybyte = SkipByte #Skip value

                        if ScreenCounter>100: #Skip 28 bytes every 100
                            if ScreenCounter>128:#Back in on-screen area
                                ScreenCounter=1#reset and don't change pixel color
                            else:
                                mybyte = SkipByte #make it a skip pixel no matter what

                        #RLE ROUTINE
                        if RLEcount>=MAX_BYTE_COUNT:
                            if TriMatch==True: #we have a tripixel
                                TriMatch = False
                                if LastTriMatch==True: #Tripixel is all the same as last color used
                                    RLEcount+=2#?? #just let the RLE do it
                                    LastTriMatch=False
                                    LastTriMatchCountOut+=1
                                else:
                                    g.write(bytes([TriNumber])) #nope, new color in there
                                #Always output a RLE
                                myout = RLEByte | RLEcount
                                g.write(bytes([myout]))
                                prev_byte = mybyte
                                RLEcount = 0  # ??
                                if mybyte != SkipByte:
                                    StartTri = True
                            else:
                                #Is this always a skip now?
                                if RLEcount==MAX_SKIP_COUNT:
                                    myout = SkipByte | RLEcount
                                    g.write(bytes([myout]))


                                    prev_byte = mybyte
                                    RLEcount = 0#??
                                    if mybyte != SkipByte:
                                        StartTri=True
                                # else:
                                #     RLEcount += 1


                        if mybyte != prev_byte:

                            if TriMatch==True:
                                if LastTriMatch==True:
                                    RLEcount+=2#??
                                    LastTriMatch=False
                                    LastTriMatchCountOut += 1
                                else:
                                    g.write(bytes([TriNumber]))
                                    cntTri += 1


                                TriMatch=False
                                if RLEcount > 0:
                                    myout = RLEByte | RLEcount
                                    g.write(bytes([myout]))

                            else:
                                # Is this always a skip now?
                                if RLEcount > 0:
                                    myout = SkipByte | RLEcount
                                    # g.write(bytes([prev_byte]))
                                    g.write(bytes([myout]))


                            prev_byte = mybyte
                            RLEcount = 1
                            if mybyte != SkipByte:
                                StartTri=True
                        else:
                            RLEcount += 1
                        #End Not StartTri
                    #READ BYTES
                    Previousbyte1  = PreviousFrame.read(1)
                    Currentbyte1  = CurrentFrame.read(1)
                    ScreenCounter += 1
                #End While Previousbyte1!=b''

                #print(FileCounter,"Skip:",cntSkip,"Tri:",cntTri)
                #print("LastTriMatch", TriMatchCount)

                FileCounter += 1

            #End Files
    print("TriMatchCount", TriMatchCount)
    print("LastTriMatchCountOut", LastTriMatchCountOut)
    #End output

main()

