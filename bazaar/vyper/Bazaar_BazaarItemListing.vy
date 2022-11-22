# @version ^0.3.5

interface ItemListing:
    def __init__(itemName: String[100], itemPrice: int128, seller: address, parentContractAddress: address, partyA: address, partyB: address): nonpayable

interface Workbench:
    def ContractCreated(): nonpayable
    def ContractUpdated(action: String[100]): nonpayable

enum StateType:
    PartyProvisioned
    ItemListed
    CurrentSaleFinalized

State: public(StateType)

InstancePartyA: public(address)
PartyABalance: public(int128)

InstancePartyB: public(address)
PartyBBalance: public(int128)

InstanceBazaarMaintainer: public(address)
CurrentSeller: public(address)

ItemName: public(String[100])
ItemPrice: public(int128)

currentItemListing: ItemListing
CurrentContractAddress: public(address)

work: Workbench

@external
def __init__(partyA: address, balanceA: int128, partyB: address, balanceB: int128):
    self.InstanceBazaarMaintainer = msg.sender
    assert partyA != partyB, "Revert"
    self.InstancePartyA = partyA
    self.PartyABalance = balanceA
    self.InstancePartyB = partyB
    self.PartyBBalance = balanceB
    self.CurrentContractAddress = self
    self.State = StateType.PartyProvisioned
    self.work = Workbench(self)
    self.work.ContractCreated()   

@external
def HasBalance(buyer: address, itemPrice: int128) -> bool:
    if buyer == self.InstancePartyA:
        return self.PartyABalance >= itemPrice
    if buyer == self.InstancePartyB:
        return self.PartyBBalance >= itemPrice
    return False

@internal
def ChangeBalance(party: address, balanceValue: int128):
    if party == self.InstancePartyA:
        self.PartyABalance = self.PartyABalance + balanceValue
    if party == self.InstancePartyB:
        self.PartyBBalance = self.PartyBBalance + balanceValue

@external
def UpdateBalance(sellerParty: address, buyerParty: address, itemPrice: int128):
    self.ChangeBalance(sellerParty, itemPrice)
    self.ChangeBalance(buyerParty, -itemPrice)
    self.State = StateType.CurrentSaleFinalized
    self.work.ContractUpdated("UpdateBalance") 

@external
def ListItem(itemName: String[100], itemPrice: int128):
    self.CurrentSeller = msg.sender
    currentItemListingLocal: ItemListing = ItemListing(self)
    currentItemListingLocal.__init__(itemName, itemPrice, self.CurrentSeller, self.CurrentContractAddress, self.InstancePartyA, self.InstancePartyB)
    self.State = StateType.ItemListed
    self.work.ContractUpdated("ListItem") 
