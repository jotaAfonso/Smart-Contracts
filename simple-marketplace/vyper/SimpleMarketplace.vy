# @version ^0.3.5

enum StateType:
    ITEMAVAILABLE
    OFFERPLACED
    ACCEPTED

instanceOwner: public(address)
description: public(String[100])
askingPrice: public(int128)
state: public(StateType)

instanceBuyer: public(address)
offerPrice: public(int128)

@external
def __init__(descriptionValue: String[100], price: int128):
    self.instanceOwner = msg.sender
    self.askingPrice = price
    self.description = descriptionValue
    self.state = StateType.ITEMAVAILABLE


@external
def MakeOffer(offerPriceValue: int128):
    # passes if its higher than zero
    # if not cond:
    #   raise "reason"
    assert offerPriceValue > 0, "Value too low"
    # passes if the item is available
    assert self.state == StateType.ITEMAVAILABLE, "State unavailable"
    # passes if the sender is not the owner
    assert self.instanceOwner != msg.sender, "Owner can not buy the item"
    self.instanceBuyer = msg.sender
    self.offerPrice = offerPriceValue
    self.state = StateType.OFFERPLACED

@external
def Reject():
    # passes if the item is with an offer
    assert self.state == StateType.OFFERPLACED, "State Incompatible"
    # passes if the item owner is the sender
    assert self.instanceOwner == msg.sender, "Buyer can not reject the offer"
    self.instanceBuyer = 0x0000000000000000000000000000000000000000
    self.state = StateType.ITEMAVAILABLE

@external
def AcceptOffer():
    # passes if the item is with an offer
    assert self.state == StateType.OFFERPLACED, "State Incompatible"
    # passes if the item owner is the sender
    assert self.instanceOwner == msg.sender, "Buyer can not accept the offer"
    self.state = StateType.ACCEPTED