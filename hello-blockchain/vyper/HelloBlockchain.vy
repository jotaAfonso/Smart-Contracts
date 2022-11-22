# @version ^0.3.5

enum StateType:
    REQUEST
    RESPOND

state: public(StateType)
requestor: public(address)
responder: public(address)

requestMessage: public(String[100])
responseMessage: public(String[100])

@external
def __init__(messageValue: String[100]):
    self.requestor = msg.sender
    self.requestMessage = messageValue
    self.state = StateType.REQUEST

@external
def SendRequest(messageRequestValue: String[100]):
    assert self.requestor == msg.sender, "Invalid Requestor"
    # passes if the state is not request 
    assert self.state != StateType.REQUEST, "Invalid State"
    self.requestMessage = messageRequestValue
    self.state = StateType.REQUEST

@external
def SendResponse(messageResponseValue: String[100]):
    # if for example the requestor is different than the responder
    #assert self.requestor != msg.sender, "Invalid Responder"
    # passes if the state is request 
    assert self.state == StateType.REQUEST, "Invalid State"
	# currently not used
    self.responder = msg.sender
    self.responseMessage = messageResponseValue
    self.state = StateType.RESPOND