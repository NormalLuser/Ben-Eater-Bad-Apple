import argparse
import os

#Fifty1Ford/NormalLuser Bad Apple Run Length Encoding (RLE) routine.
#Modified version of: from :


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

    MAX_BYTE_COUNT = 255
    #output_file = "BadApple47NoVsyncNoBeep2.bin"
    output_file = r"C:\Users\Andrew\OneDrive\Dokumente\Arduino\Assembly\Ben-Eater-Bad-Apple-main\BE_Bad_Apple\SteamboatWillieExtraBW.bin"
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


    #vars for Lookup for TriBytes
    #This version of the encoder will just assume valid B/W/G/G colors.
    #I can make this fancy later (or just use older encoder for color content)

    StartTri=False #True #??First bytes of first image should be a Tri??
    TriCount=0
    TriMatch=False
    TriNumber=0
    TriByte=[0,0,0]

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

    ScreenCounter = 1  # we read First byte of frame, We know this is first position in stream
    output_file = r"C:\Users\Andrew\OneDrive\Dokumente\Arduino\Assembly\Ben-Eater-Bad-Apple-main\BE_Bad_Apple\SteamboatWillieExtraBW.bin"
    path = r"C:\Users\Andrew\OneDrive\Dokumente\Arduino\Assembly\Ben-Eater-Bad-Apple-main\BE_Bad_Apple\SteamBoatExtraBW"
    
    if os.path.isfile(output_file):
        os.remove(output_file)
    
    FileCounter = 30
    ScreenCounter = 1
    MAX_BYTE_COUNT = 255
    
    with open(output_file, 'ba') as g:
        while FileCounter < 6570:
            previous_frame_path = os.path.join(path, f"BadApple{FileCounter}.txt")
            current_frame_path = os.path.join(path, f"BadApple{FileCounter + 1}.txt")
    
            try:
                with open(previous_frame_path, "rb") as PreviousFrame, open(current_frame_path, "rb") as CurrentFrame:
                    # Initialize frame variables
                    Previousbyte1 = PreviousFrame.read(1)
                    Currentbyte1 = CurrentFrame.read(1)
    
                    if FirstSkip == 1:
                        FirstSkip = 0
                        for _ in range(7):  # Skip 7 bytes
                            Previousbyte1 = PreviousFrame.read(1)
                            Currentbyte1 = CurrentFrame.read(1)
    
                    while Previousbyte1 != b'':
                        if StartTri:
                            # Handle TriByte logic
                            TriByte[TriCount] = prev_byte
                            TriCount += 1
                            mybyte = int.from_bytes(Currentbyte1, "little")
                            prev_byte = mybyte
                            VChangeCounter += 1
                            if TriCount == 3:
                                StartTri = False
                                TriCount = 0
                                CheckBytes = TriByte[0], TriByte[1], TriByte[2]
                                for y in range(len(PixelList)):
                                    ByteA, ByteB, ByteC, ByteD = PixelList[y]
                                    if CheckBytes == (ByteA, ByteB, ByteC):
                                        TriMatch = True
                                        TriNumber = ByteD + 65
                                        RLEcount = 1
                                        mybyte = TriByte[2]
                                        prev_byte = mybyte
                                        break
                            mybyte = int.from_bytes(Currentbyte1, "little")
    
                        if not StartTri:
                            mybyte = int.from_bytes(Currentbyte1, "little")
    
                            if Previousbyte1 == Currentbyte1:
                                mybyte = SkipByte
    
                            if ScreenCounter > 100:
                                if ScreenCounter > 128:
                                    ScreenCounter = 1
                                else:
                                    mybyte = SkipByte
    
                            if RLEcount == MAX_BYTE_COUNT:
                                g.write(bytes([RLEcount]))
                                if TriMatch:
                                    g.write(bytes([TriNumber]))
                                    TriMatch = False
                                else:
                                    g.write(bytes([prev_byte]))
    
                                prev_byte = mybyte
                                RLEcount = 0
                                if mybyte != SkipByte:
                                    StartTri = True
    
                            if mybyte != prev_byte:
                                g.write(bytes([RLEcount]))
                                if TriMatch:
                                    g.write(bytes([TriNumber]))
                                    TriMatch = False
                                else:
                                    g.write(bytes([prev_byte]))
    
                                prev_byte = mybyte
                                RLEcount = 1
                                if mybyte != SkipByte:
                                    StartTri = True
                            else:
                                RLEcount += 1
    
                        # Read the next byte
                        Previousbyte1 = PreviousFrame.read(1)
                        Currentbyte1 = CurrentFrame.read(1)
                        ScreenCounter += 1
    
            except FileNotFoundError as e:
                print(f"File not found: {e}")
                break
            except Exception as e:
                print(f"Unexpected error: {e}")
                break
    
            FileCounter += 1






            #Below is to adjust centering
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
                #mybyte = int.from_bytes(Currentbyte1, "little")
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
                        for y in range(len(PixelList)):
                            ByteA = PixelList[y][0]
                            ByteB = PixelList[y][1]
                            ByteC = PixelList[y][2]
                            ByteD = PixelList[y][3]
                            MYPixelListCheck = ByteA, ByteB, ByteC

                            if CheckBytes == MYPixelListCheck:
                                TriMatch = True
                                TriNumber = ByteD + 65  # PixelList[y][3]
                                RLEcount = 1#count = 0  # not sure if 0 or 1??
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
                    if RLEcount==MAX_BYTE_COUNT:
                        #write_byte(output_file, RLEcount, prev_byte)
                        g.write(bytes([RLEcount]))
                        if TriMatch==True:
                            g.write(bytes([TriNumber]))
                            TriMatch = False
                        else:
                            g.write(bytes([prev_byte]))

                            if prev_byte==64:
                                cntSkip+=1
                            if prev_byte>64:
                                cntTri+=1
                                cntTri+=1

                        prev_byte = mybyte
                        RLEcount = 0#??
                        if mybyte != SkipByte:
                            StartTri=True

                    if mybyte != prev_byte:
                        #write_byte(output_file, RLEcount, prev_byte)
                        g.write(bytes([RLEcount]))
                        if TriMatch==True:
                            g.write(bytes([TriNumber]))
                            cntTri += 1
                            TriMatch=False
                        else:
                            g.write(bytes([prev_byte]))
                            if prev_byte==64:
                                cntSkip+=1
                            if prev_byte>64:
                                cntTri+=1

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

            FileCounter += 1            #g.write(bytes([RLEcount]))

main()

