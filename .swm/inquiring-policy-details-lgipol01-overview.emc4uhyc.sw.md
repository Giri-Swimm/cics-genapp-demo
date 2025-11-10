---
title: Inquiring Policy Details (LGIPOL01) - Overview
---
# Overview

This document explains the flow of insurance policy inquiry. Incoming requests are validated, errors are logged with context, and inquiries are routed to retrieve full details for Endowment, House, or Motor policies. Only valid requests receive policy data; errors are logged and returned as needed.

```mermaid
flowchart TD
    node1["Initializing and Validating Transaction Context"]:::HeadingStyle --> node2{"Is input data valid?"}
    click node1 goToHeading "Initializing and Validating Transaction Context"
    node2 -->|"No"| node5["Policy Type Dispatch and Data Retrieval
(Return error: invalid input)
(Policy Type Dispatch and Data Retrieval)"]:::HeadingStyle
    node2 -->|"Yes"| node3["Preparing and Routing Policy Inquiry"]:::HeadingStyle
    click node3 goToHeading "Preparing and Routing Policy Inquiry"
    node3 --> node4{"Is policy type supported and policy found with sufficient buffer?
(Policy Type Dispatch and Data Retrieval)"}:::HeadingStyle
    click node4 goToHeading "Policy Type Dispatch and Data Retrieval"
    node4 -->|"Yes"| node6["Policy Type Dispatch and Data Retrieval
(Return policy details)
(Policy Type Dispatch and Data Retrieval)"]:::HeadingStyle
    node4 -->|"No"| node5
    click node5 goToHeading "Policy Type Dispatch and Data Retrieval"
    click node6 goToHeading "Policy Type Dispatch and Data Retrieval"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Programs

- LGIPOL01 (<SwmPath>[base/src/lgipol01.cbl](base/src/lgipol01.cbl)</SwmPath>)
- LGIPDB01 (<SwmPath>[base/src/lgipdb01.cbl](base/src/lgipdb01.cbl)</SwmPath>)
- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)

### Copybooks

- SQLCA
- LGPOLICY (<SwmPath>[base/src/lgpolicy.cpy](base/src/lgpolicy.cpy)</SwmPath>)
- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)

# Where is this program used?

This program is used multiple times in the codebase as represented in the following diagram:

```mermaid
graph TD
  roqcq("Managing Commercial Insurance Policies (LGTESTP4)") --> 5byqz("Inquiring Policy Details (LGIPOL01)"):::currentEntity
click roqcq openCode "base/src/lgtestp4.cbl:1"
5tnwu("Endowment Policy Menu (LGTESTP2)") --> 5byqz("Inquiring Policy Details (LGIPOL01)"):::currentEntity
click 5tnwu openCode "base/src/lgtestp2.cbl:1"
whbw8("Motor Policy Menu (LGTESTP1)") --> 5byqz("Inquiring Policy Details (LGIPOL01)"):::currentEntity
click whbw8 openCode "base/src/lgtestp1.cbl:1"
ghguj("House Policy Menu (LGTESTP3)") --> 5byqz("Inquiring Policy Details (LGIPOL01)"):::currentEntity
click ghguj openCode "base/src/lgtestp3.cbl:1"
  
  
click 5byqz openCode "base/src/lgipol01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   roqcq("Managing Commercial Insurance Policies (LGTESTP4)") --> 5byqz("Inquiring Policy Details (LGIPOL01)"):::currentEntity
%% click roqcq openCode "<SwmPath>[base/src/lgtestp4.cbl](base/src/lgtestp4.cbl)</SwmPath>:1"
%% 5tnwu("Endowment Policy Menu (LGTESTP2)") --> 5byqz("Inquiring Policy Details (LGIPOL01)"):::currentEntity
%% click 5tnwu openCode "<SwmPath>[base/src/lgtestp2.cbl](base/src/lgtestp2.cbl)</SwmPath>:1"
%% whbw8("Motor Policy Menu (LGTESTP1)") --> 5byqz("Inquiring Policy Details (LGIPOL01)"):::currentEntity
%% click whbw8 openCode "<SwmPath>[base/src/lgtestp1.cbl](base/src/lgtestp1.cbl)</SwmPath>:1"
%% ghguj("House Policy Menu (LGTESTP3)") --> 5byqz("Inquiring Policy Details (LGIPOL01)"):::currentEntity
%% click ghguj openCode "<SwmPath>[base/src/lgtestp3.cbl](base/src/lgtestp3.cbl)</SwmPath>:1"
%%   
%%   
%% click 5byqz openCode "<SwmPath>[base/src/lgipol01.cbl](base/src/lgipol01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1nZW5hcHAtZGVtbyUzQSUzQXN3aW1taW8=" repo-name="cics-genapp-demo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
