# @version ^0.3.5

enum StateType:
    CREATE
    COMPUTETOTAL

manufacturer: public(address)
total : public(int128)
defectiveComponentsCount: DynArray[uint256, 12]
# why is it not recognizing StateType
state: public(StateType)

@external
def __init__(defectiveComponentsCountValue: uint256[12]):
    self.manufacturer = msg.sender
    self.defectiveComponentsCount = defectiveComponentsCountValue
    self.state = StateType.CREATE

@external
def ComputeTotal():
    assert self.manufacturer == msg.sender, "Revert"
    for i in range(12):
        self.total = selftotal + self.defectiveComponentsCount[i]

@external
def GetDefectiveComponentsCount() -> DynArray[uint256, 12]:
    return self.defectiveComponentsCount