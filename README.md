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

![Topology](https://github.com/epkboan/epkboan.github.io/blob/master/topology.png?raw=true "Topology")


### Screenshot of weather data
A simple 7" tablet (i use an Asus Memo Pad) is always on and displays the current weather data (just like any other weather station). Once every minute a new query is made to the homautomation webserver (raspberry pi), the webpage is assembled dynamically with the latest info. The top part is from the local txrx device and the google chart diagram is from a mysql query (of past txrx data) 
![ss](https://github.com/epkboan/epkboan.github.io/blob/master/homeautomation_ss.jpg?raw=true "Screenshot weather")

### Screenshots of device contro
I use a mixture of standard power outlets and own built to control garden sprinklers etc
![ss1](https://github.com/epkboan/epkboan.github.io/blob/master/mobile1.png?raw=true "Screenshot mobile 1")

![ss2](https://github.com/epkboan/epkboan.github.io/blob/master/mobile2.png?raw=true "Screenshot mobile 2")


