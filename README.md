homeautomation
==============
Home Automation for devices and sensors (using the txrx project) with web server interface 

![Topology](https://github.com/epkboan/epkboan.github.io/blob/master/topology.png?raw=true "Topology")

install erlang (rasberry pi):
Follow the instructions to download erlang-mini
http://www.erlang-embedded.com/
and you also need:
```
sudo apt-get install erlang-dev erlang-edoc erlang-eunit
```

Clone this project and then navigate into the project and to "make". This will first download the other projects that homeautomation is depending on (web server, nitrogen webframework etc).
```
git clone https://github.com/epkboan/homeautomation.git
cd homeautomation
make
```

##### Firmware
Next you need to install the firmware for the arduino UNO
First install the ino build system
http://inotool.org/#installation

##### Compile the arduino firmware
```
cd homeautomation/deps/txrx/arduino/
ino build
```

If all went well
```
ino upload
```





### Screenshot of weather data
A simple 7" tablet (i use an Asus Memo Pad) is always on and displays the current weather data (just like any other weather station). Once every minute a new query is made to the homautomation webserver (raspberry pi), the webpage is assembled dynamically with the latest info. The top part is from the local txrx device and the google chart diagram is from a mysql query (of past txrx data) 


![ss](https://github.com/epkboan/epkboan.github.io/blob/master/homeautomation_ss.jpg?raw=true "Screenshot weather")

### Screenshots of device control
I use a mixture of standard power outlets and own built to control garden sprinklers etc. The devices are controlled either manually via the web interface as seen below or as programmable timers (see etc/homeautomation.cfg for a sample, but it is just a list of on/off times of any length).

At the bottom of the page the current sensor values are displayed 

![ss1](https://github.com/epkboan/epkboan.github.io/blob/master/mobile1.png "Screenshot mobile 1")



