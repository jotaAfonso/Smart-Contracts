# @version ^0.3.5

enum StateType:
    SETFLYERANDREWARD
    MILESADDED

state: public(StateType)
airlineRepresentative: public(address)
flyer: public(address)
rewardsPerMile: public(uint256) # 0 to 2 ** 256 - 1
miles: DynArray[uint256, 100] # in this implementation this could be fixed instead
indexCalculatedUpto: public(uint256)
totalRewards: public(uint256)
milesCounter: public(uint256)

@external
def __init__(flyer: address, rewardsPerMile: int128):
    self.airlineRepresentative = msg.sender
    self.flyer = flyer
    self.rewardsPerMile = convert(rewardsPerMile, uint256)
    self.indexCalculatedUpto = 0
    self.totalRewards = 0
    self.state = StateType.SETFLYERANDREWARD
    self.miles = []
    self.milesCounter = 0

# could probably improve by being private and giving the size as and argument in another function
@external
def AddMiles(milesValues: DynArray[uint256, 50]):
    assert self.flyer == msg.sender, "Revert"
    assert len(milesValues) + self.milesCounter <= len(self.miles), "Reached Max"
    self.milesCounter = len(milesValues) + self.milesCounter
    for mileLoop in milesValues:
        self.miles.append(mileLoop)
    self.ComputeTotalRewards()
    self.state = StateType.MILESADDED

@internal
def ComputeTotalRewards():
    starRange: uint256 = self.indexCalculatedUpto
    plusRange: uint256 = len(self.miles) - starRange
    assert starRange > 0 
    assert plusRange > 0
    # for i in range(a, a + N):
    for i in range(starRange, starRange + 10):
        self.totalRewards = self.totalRewards + self.rewardsPerMile * self.miles[i]
        # TODO check if it messes with the range
        self.indexCalculatedUpto = self.indexCalculatedUpto + 1 

@external
def GetMiles() -> DynArray[uint256, 100]:
    assert self.state == StateType.MILESADDED, "No miles added"
    return self.miles
