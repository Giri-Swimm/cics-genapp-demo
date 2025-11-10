---
title: Deleting Policy Business Logic (LGDPOL01) - Overview
---
# Overview

This document describes the flow for deleting insurance policies. The process validates incoming requests, coordinates the removal of policy data from both the database and related VSAM records, and logs all actions for traceability. The flow returns a confirmation or an error code to the caller.

```mermaid
flowchart TD
    node1["Starting Request Validation and Setup"]:::HeadingStyle --> node2["Validating Input and Request Type"]:::HeadingStyle
    node2 -->|"Invalid or unsupported request"| node5["Coordinating Policy Deletion and Error Handling"]:::HeadingStyle
    node2 -->|"Valid request"| node3["Triggering Database Policy Deletion"]:::HeadingStyle
    node3 -->|"Database deletion failed"| node5
    node3 -->|"Database deletion succeeded"| node4["Cleaning Up VSAM Policy Records and Logging"]:::HeadingStyle
    node4 -->|"VSAM cleanup complete"| node5

    click node1 goToHeading "Starting Request Validation and Setup"
    click node2 goToHeading "Validating Input and Request Type"
    click node3 goToHeading "Triggering Database Policy Deletion"
    click node4 goToHeading "Cleaning Up VSAM Policy Records and Logging"
    click node5 goToHeading "Coordinating Policy Deletion and Error Handling"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Programs

- LGDPOL01 (<SwmPath>[base/src/lgdpol01.cbl](base/src/lgdpol01.cbl)</SwmPath>)
- LGDPDB01 (<SwmPath>[base/src/lgdpdb01.cbl](base/src/lgdpdb01.cbl)</SwmPath>)
- LGDPVS01 (<SwmPath>[base/src/lgdpvs01.cbl](base/src/lgdpvs01.cbl)</SwmPath>)
- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)

### Copybooks

- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)
- SQLCA

# Where is this program used?

This program is used multiple times in the codebase as represented in the following diagram:

```mermaid
graph TD
  oka93("Managing Commercial Insurance Policies (LGTESTP4)") --> r6tpu("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity
click oka93 openCode "base/src/lgtestp4.cbl:1"
mw4h9("Endowment Policy Menu (LGTESTP2)") --> r6tpu("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity
click mw4h9 openCode "base/src/lgtestp2.cbl:1"
a1wz6("Motor Policy Menu (LGTESTP1)") --> r6tpu("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity
click a1wz6 openCode "base/src/lgtestp1.cbl:1"
8pvb5("House Policy Menu (LGTESTP3)") --> r6tpu("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity
click 8pvb5 openCode "base/src/lgtestp3.cbl:1"
  
  
click r6tpu openCode "base/src/lgdpol01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   oka93("Managing Commercial Insurance Policies (LGTESTP4)") --> r6tpu("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity
%% click oka93 openCode "<SwmPath>[base/src/lgtestp4.cbl](base/src/lgtestp4.cbl)</SwmPath>:1"
%% mw4h9("Endowment Policy Menu (LGTESTP2)") --> r6tpu("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity
%% click mw4h9 openCode "<SwmPath>[base/src/lgtestp2.cbl](base/src/lgtestp2.cbl)</SwmPath>:1"
%% a1wz6("Motor Policy Menu (LGTESTP1)") --> r6tpu("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity
%% click a1wz6 openCode "<SwmPath>[base/src/lgtestp1.cbl](base/src/lgtestp1.cbl)</SwmPath>:1"
%% 8pvb5("House Policy Menu (LGTESTP3)") --> r6tpu("Deleting Policy Business Logic (LGDPOL01)"):::currentEntity
%% click 8pvb5 openCode "<SwmPath>[base/src/lgtestp3.cbl](base/src/lgtestp3.cbl)</SwmPath>:1"
%%   
%%   
%% click r6tpu openCode "<SwmPath>[base/src/lgdpol01.cbl](base/src/lgdpol01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1nZW5hcHAtZGVtbyUzQSUzQXN3aW1taW8=" repo-name="cics-genapp-demo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
