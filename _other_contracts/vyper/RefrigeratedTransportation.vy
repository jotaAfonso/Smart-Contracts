# @version ^0.3.5

enum StateType:
    CREATESTATE
    INTRANSITSTATE
    COMPLETEDSTATE
    OUTOFCOMPLIANCESTATE

enum SensorType:
    NONESENSOR
    HUMIDITYSENSOR
    TEMPERATURESENSOR
    
state: public(StateType)
sensor: public(SensorType)

owner: public(address)
initiatingCounterparty: public(address)
counterparty: public(address)
previousCounterparty: public(address)
device: public(address)
supplyChainOwner: public(address)
supplyChainObserver: public(address)

minHumidity : public(int128)
maxHumidity : public(int128)
minTemperature : public(int128)
maxTemperature : public(int128)
complianceSensorReading : public(int128)
lastSensorUpdateTimestamp : public(int128)

complianceStatus: public(bool)
complianceDetail: public(String[100])

@external
def __init__(device: address, supplyChainOwner: address, supplyChainObserver: address, minHumidity: int128, maxHumidity: int128, minTemperature: int128, maxTemperature: int128):
    self.complianceStatus = True
    self.complianceSensorReading = -1
    self.initiatingCounterparty = msg.sender
    self.owner = self.initiatingCounterparty
    self.counterparty = self.initiatingCounterparty
    self.device = device
    self.supplyChainOwner = supplyChainOwner
    self.supplyChainObserver = supplyChainObserver
    self.minHumidity = minHumidity
    self.maxHumidity = maxHumidity
    self.minTemperature = minTemperature
    self.maxTemperature = maxTemperature
    self.state = StateType.CREATESTATE
    self.complianceDetail = "N/A"

@external
def IngestTelemetry(humidity: int128, temperature: int128, timestampValue: int128):
    assert self.state != StateType.COMPLETEDSTATE, "Revert"
    assert self.state != StateType.OUTOFCOMPLIANCESTATE, "Revert"
    assert self.device == msg.sender, "Revert"
   
    self.lastSensorUpdateTimestamp = timestampValue

    if humidity > self.maxHumidity or humidity < self.minHumidity:
        self.sensor = SensorType.HUMIDITYSENSOR
        self.complianceSensorReading = humidity
        self.complianceDetail = "Humidity value out of range."
        self.complianceStatus = False

    elif temperature > self.maxTemperature or temperature < self.minTemperature:
        self.sensor = SensorType.TEMPERATURESENSOR
        self.complianceSensorReading = temperature
        self.complianceDetail = "Temperature value out of range."
        self.complianceStatus = False

    assert self.complianceStatus != False, "Revert"
    self.state = StateType.OUTOFCOMPLIANCESTATE
    
@external
def TransferResponsibility(newCounterparty: address):
    assert self.state != StateType.COMPLETEDSTATE, "Revert"
    assert self.state != StateType.OUTOFCOMPLIANCESTATE, "Revert"
    assert self.initiatingCounterparty == msg.sender and self.counterparty == msg.sender, "Revert"
    assert newCounterparty != self.device, "Revert"
    assert self.state == StateType.CREATESTATE

    self.previousCounterparty = self.counterparty
    self.counterparty = newCounterparty

@external
def Complete():
    assert self.state != StateType.COMPLETEDSTATE, "Revert" 
    assert self.state != StateType.OUTOFCOMPLIANCESTATE, "Revert" 
    assert self.owner == msg.sender and self.supplyChainObserver == msg.sender, "Revert" 

    self.state = StateType.COMPLETEDSTATE
    self.previousCounterparty = self.counterparty
    self.counterparty = 0x0000000000000000000000000000000000000000