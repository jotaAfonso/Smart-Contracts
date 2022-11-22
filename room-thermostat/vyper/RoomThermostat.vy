# @version ^0.3.5

TARGET: constant(int128) = 70

enum StateType:
    CREATED
    INUSE

enum ModeType:
    OFF
    COOL
    HEAT
    AUTO

state: public(StateType)
installer: public(address)
user: public(address)
targetTemperature: public(int128) # -2 ** 127 to (2 ** 127 - 1)
mode: public(ModeType)

@external
def __init__(thermostatInstaller: address, thermostatUser: address):
    self.installer = thermostatInstaller
    self.user = thermostatUser
    self.targetTemperature = TARGET
    # missing from the example
    self.state = StateType.CREATED

@external
def StartThermostat():
    assert self.installer == msg.sender and self.state == StateType.CREATED, "Was not created or it was not the installer"
    self.state = StateType.INUSE

@external
def SetTargetTemperature(targetTemperatureValue: int128):
    assert self.user == msg.sender and self.state == StateType.INUSE, "Cannot set temperature"
    self.targetTemperature = targetTemperatureValue

@external
def SetMode(modeValue: ModeType):
    assert self.user == msg.sender and self.state == StateType.INUSE
    self.mode = modeValue