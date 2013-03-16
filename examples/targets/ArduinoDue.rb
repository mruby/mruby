# Cross Compiling configuration for Arduino Duea
# http://arduino.cc/en/Main/ArduinoBoardDue
#
# Requires Arduino IDE >= 1.5
MRuby::CrossBuild.new("Arduino Due") do |conf|
  toolchain :gcc

  # GNU Linux
  ARDUINO_PATH = '/opt/arduino'
  BIN_PATH = "#{ARDUINO_PATH}/hardware/tools/g++_arm_none_eabi/bin"
  SAM_PATH = "#{ARDUINO_PATH}/hardware/arduino/sam"
  TARGET_PATH = "#{SAM_PATH}/variants/arduino_due_x"

  conf.cc do |cc|
    cc.command = "#{BIN_PATH}/arm-none-eabi-gcc"
    cc.include_paths = ["#{SAM_PATH}/system/libsam -I#{SAM_PATH}/system/CMSIS/CMSIS/Include/",
                        "#{SAM_PATH}/system/CMSIS/Device/ATMEL/",
                        "#{SAM_PATH}/cores/arduino -I#{TARGET_PATH}",
                        "#{MRUBY_ROOT}/include"]
    cc.flags << '-g -Os -w -ffunction-sections -fdata-sections -nostdlib --param max-inline-insns-single=500 ' +
                '-Dprintf=iprintf -mcpu=cortex-m3 -DF_CPU=84000000L -DARDUINO=152 -D__SAM3X8E__ -mthumb -DUSB_PID=0x003e -DUSB_VID=0x2341 -DUSBCON'
    cc.compile_options = "%{flags} -o %{outfile} -c %{infile}"
  end

  conf.archiver do |archiver|
    archiver.command = "#{BIN_PATH}/arm-none-eabi-ar"
    archiver.archive_options = 'rcs %{outfile} %{objs}'
  end

  # No binaries necessary
  conf.bins = []
end
