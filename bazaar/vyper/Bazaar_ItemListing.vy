# @version ^0.3.5

interface Bazaar:
    def HasBalance(InstanceBuyer: address, ItemPrice: int128) -> bool: view
    def UpdateBalance(Seller: address, InstanceBuyer: address, ItemPrice: int128): nonpayable

interface Workbench:
    def ContractCreated(): nonpayable
    def ContractUpdated(action: String[100]): nonpayable

enum StateType:
    ItemAvailable
    ItemSold

Seller: public(address)
InstanceBuyer: public(address)
ParentContract: public(address)

ItemName: public(String[100])
ItemPrice: public(int128)
State: public(StateType)

PartyA: public(address)
PartyB: public(address)

work: Workbench

@external
def __init__(itemName: String[100], itemPrice: int128, seller: address, parentContractAddress: address, partyA: address, partyB: address):
    self.Seller = seller
    self.ParentContract = parentContractAddress
    self.ItemName = itemName
    self.ItemPrice = itemPrice
    self.PartyA = partyA
    self.PartyB = partyB
    self.State = StateType.ItemAvailable
    self.work = Workbench(self)
    self.work.ContractCreated()    

@external 
def BuyItem():
    self.InstanceBuyer = msg.sender
    assert self.Seller != self.InstanceBuyer, "Revert"
    bazaar: Bazaar = Bazaar(self.ParentContract)
    assert bazaar.HasBalance(self.InstanceBuyer, self.ItemPrice), "Revert"
    bazaar.UpdateBalance(self.Seller, self.InstanceBuyer, self.ItemPrice)
    self.State = StateType.ItemSold
    self.work = Workbench(self.ParentContract)
    self.work.ContractUpdated("BuyItem")
