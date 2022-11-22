# @version ^0.3.5

enum StateType:
    CREATED
    INTRANSIT
    COMPLETED

state: public(StateType)
initiatingCounterparty: public(address)
counterparty: public(address)
previousCounterparty: public(address)
supplyChainOwner: public(address)
supplyChainObserver: public(address)


@external
def __init__(supplyChainOwner: address, supplyChainObserver: address):
    self.initiatingCounterparty = msg.sender;
    self.counterparty = self.initiatingCounterparty;
    self.supplyChainOwner = supplyChainOwner;
    self.supplyChainObserver = supplyChainObserver;
    self.state = StateType.CREATED;

@external
def TransferResponsibility(newCounterparty: address):
    assert self.counterparty == msg.sender and self.state != StateType.COMPLETED, "Revert"
    if self.state == StateType.CREATED
        self.state = StateType.INTRANSIT
    self.previousCounterparty = self.counterparty
    self.counterparty = newCounterparty

@external
def Complete():
    assert self.supplyChainOwner == msg.sender and self.state == StateType.INTRANSIT, "Revert"
    self.state = StateType.COMPLETED
    self.previousCounterparty = self.counterparty
    self.counterparty = 0x0000000000000000000000000000000000000000