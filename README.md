homeautomation
==============

Home Automation for devices and sensors (using the txrx project) with web server interface 

install erlang (rasberry pi):
Follow the instructions to download erlang-mini
http://www.erlang-embedded.com/
and you also need:
sudo apt-get install erlang-dev erlang-edoc erlang-eunit

Clone this project
then do "make"

You now need to install the firmware onto the arduino UNO
First install the ino build system
http://inotool.org/#installation

Compile the arduino firmware
cd homeautomation/deps/txrx/arduino/
ino build
If all went good:
ino upload



