# @version ^0.3.5

event WorkbenchContractCreated:
    applicationName: String[100]
    workflowName: String[100]
    originatingAddress: address

event WorkbenchContractUpdated:
    applicationName: String[100]
    workflowName: String[100]
    action: String[100]
    originatingAddress: address

ApplicationName: String[100]
WorkflowName: String[100]

@external
def __init__(applicationName: String[100], workflowName: String[100]):
    self.ApplicationName = applicationName
    self.WorkflowName = workflowName

@internal
def ContractCreated():
    log WorkbenchContractCreated(self.ApplicationName, self.WorkflowName, msg.sender)

@internal
def ContractUpdated(action: String[100]):
    log WorkbenchContractUpdated(self.ApplicationName, self.WorkflowName, action, msg.sender)