# Ben-Eater-Bad-Apple
Bad Apple for Worlds Worst Video Card + 6502 kits
6502 Assembly and PC Python based encoder

Now with 37 FPS!!! Wow! On my way!
![Bad Apple Demo](https://raw.githubusercontent.com/Fifty1Ford/Ben-Eater-Bad-Apple/main/BadCider.png)


Load up my BeWozmon with rom, or by using the BeEhBasic in my projects page and typing CALL $Fc00
Then from Wozmon Type L 
This will put it in INTEL HEX LOAD mode. Then 
Then drag and drop the HEX file and transfer in binary mode. (TeraTerm)
Then 1800R.

If you are running the stock Ben Eater Wozmonitor and can't load hex files you can use:
BadApple37Woz.txt

And just copy and paste it into your terminal. 
I use TeraTerm and have Transmit Delay set to 3 and 17 in Setup→Serial Port…
This works fine with my CPU clocked at 5Mhz and running in both Hsync and Vsync as recommended for this demo.
![Bad Apple Demo](https://raw.githubusercontent.com/Fifty1Ford/Ben-Eater-Bad-Apple/main/BadAppleDecoder.jpg)
Speaking of…
Demo Hardware:
Start with a Stock: 
Ben Eater 6502 Breadboard computer kit
Ben Eater Worlds Worst Video Card kit
Ben Eater Serial Adapter kit
https://eater.net/shop
(or follow along and build your own with your own parts)
Then:
Remove the little LCD for now.
Clock CPU at 5Mhz. To do this disconnect your 6502 clock wire from the 1Mhz crystal and use a jumper wire to connect your 6502 to the first counter of the VGA setup. IE a 5mhz clock. You may need to add additional bypass capacitors to your power rails and run additional wires directly to power and ground rails on the breadboards to make it stable, but the stock parts should work fine at 5mhz.

You will also need a SD card adapter connected.
Read this: https://github.com/gfoot/sdcard6502
To follow the same instructions I did.

Get the SD card thing-ie. It is just a tiny board with a micro SD card slot, chips to switch from 5 volt on the 6502 side to 3 volt on the sd card side.
I do use pull-up resistors on the sd signal pins. (Wait..  Edit that... )
I pulled the resistors and instead just connect right to the via. It works fine.


NOTE:
I use PA0 for Data In (MISO) and CA2 for the clock.
While doing this demo disconnect everything else from Port A. (Like the P/S2 keyboard)
You can use whatever you like on Port B but leave PA7 (bit 8) free for music in the future.
I happened to use bit 5  VIA pin PB4 for Data Out (MOSI) and bit 3 VIA pin PB3 for chip select.


