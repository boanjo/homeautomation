homeautomation
==============
Home Automation for devices and sensors with web server interface in erlang

![Topology](https://github.com/epkboan/epkboan.github.io/blob/master/topology.png?raw=true "Topology")

The 433MHz network is maintained by an Arduino UNO using the https://github.com/epkboan/txrx project. It listens for sensors and power outlet commands from other sources (like remote controls) to keep the state of the devices. It sends commands to the power outlets (programmable timers or user controlled). The UNO is connected via serial (USB) interface to a raspberry PI. The UNO is just a dumb bitbanging slave and the PI has the control logic and device states.

The txrx sensor data is pushed to an mysql database every 15min (runnning on an external Servage web server in my case, but it can just as well be running on the raspberry PI. And even better run a mnesia database rather than mysql). 

The web server is using the nitrogen web framework running a mochiweb. The web pages (as seen on the screenshots below) are dynamically created every time the user do a request (or refresh). The current state of devices, sensor data, and somtimes also external mysql query is collected and displayed on the page.


##### Preparations
* Prepare the raspberry pi with basic needs. Your favorite editor etc
* Install erlang (on the rasberry pi):
Follow the instructions to download erlang-mini
http://www.erlang-embedded.com/
and you also need:
```
sudo apt-get install erlang-dev erlang-edoc erlang-eunit
```
* Install the ino build system
http://inotool.org/#installation

##### Get started
Clone this project and then navigate into the project and to "make". This will first download the other projects that homeautomation is depending on (web server, nitrogen webframework etc).
```
git clone https://github.com/epkboan/homeautomation.git
cd homeautomation
make
```

##### Firmware
Next you need to install the firmware for the arduino UNO

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



