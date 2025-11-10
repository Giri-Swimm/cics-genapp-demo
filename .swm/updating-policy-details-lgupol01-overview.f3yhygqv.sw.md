---
title: Updating Policy details (LGUPOL01) - Overview
---
# Overview

This document describes the flow for updating insurance policy details. The process validates input, updates policy records in the database and VSAM file, and ensures auditability and data integrity for endowment, house, and motor insurance products.

```mermaid
flowchart TD
    node1["Starting the transaction and preparing context"]:::HeadingStyle
    click node1 goToHeading "Starting the transaction and preparing context"
    node1 --> node2{"Preparing for policy update and validating input
(Preparing for policy update and validating input)"}:::HeadingStyle
    click node2 goToHeading "Preparing for policy update and validating input"
    node2 -->|"Valid input and recognized policy type"| node3{"Updating policy details in database and handling concurrency
(Performing type-specific policy update with concurrency control)"}:::HeadingStyle
    click node3 goToHeading "Performing type-specific policy update with concurrency control"
    node2 -->|"Invalid input or unrecognized policy type"| node5["Reject update request"]
    node3 -->|"Update successful"| node4["Updating VSAM policy record and handling errors"]:::HeadingStyle
    click node4 goToHeading "Updating VSAM policy record and handling errors"
    node3 -->|"Policy not found or concurrency conflict"| node5
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Programs

- LGUPOL01 (<SwmPath>[base/src/lgupol01.cbl](base/src/lgupol01.cbl)</SwmPath>)
- LGUPDB01 (<SwmPath>[base/src/lgupdb01.cbl](base/src/lgupdb01.cbl)</SwmPath>)
- LGUPVS01 (<SwmPath>[base/src/lgupvs01.cbl](base/src/lgupvs01.cbl)</SwmPath>)
- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)

### Copybooks

- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)
- LGPOLICY (<SwmPath>[base/src/lgpolicy.cpy](base/src/lgpolicy.cpy)</SwmPath>)
- SQLCA

# Where is this program used?

This program is used multiple times in the codebase as represented in the following diagram:

```mermaid
graph TD
  e684i("Endowment Policy Menu (LGTESTP2)") --> 9il0o("Updating Policy details (LGUPOL01)"):::currentEntity
click e684i openCode "base/src/lgtestp2.cbl:1"
fc3gt("Motor Policy Menu (LGTESTP1)") --> 9il0o("Updating Policy details (LGUPOL01)"):::currentEntity
click fc3gt openCode "base/src/lgtestp1.cbl:1"
o8bp7("House Policy Menu (LGTESTP3)") --> 9il0o("Updating Policy details (LGUPOL01)"):::currentEntity
click o8bp7 openCode "base/src/lgtestp3.cbl:1"
  
  
click 9il0o openCode "base/src/lgupol01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   e684i("Endowment Policy Menu (LGTESTP2)") --> 9il0o("Updating Policy details (LGUPOL01)"):::currentEntity
%% click e684i openCode "<SwmPath>[base/src/lgtestp2.cbl](base/src/lgtestp2.cbl)</SwmPath>:1"
%% fc3gt("Motor Policy Menu (LGTESTP1)") --> 9il0o("Updating Policy details (LGUPOL01)"):::currentEntity
%% click fc3gt openCode "<SwmPath>[base/src/lgtestp1.cbl](base/src/lgtestp1.cbl)</SwmPath>:1"
%% o8bp7("House Policy Menu (LGTESTP3)") --> 9il0o("Updating Policy details (LGUPOL01)"):::currentEntity
%% click o8bp7 openCode "<SwmPath>[base/src/lgtestp3.cbl](base/src/lgtestp3.cbl)</SwmPath>:1"
%%   
%%   
%% click 9il0o openCode "<SwmPath>[base/src/lgupol01.cbl](base/src/lgupol01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1nZW5hcHAtZGVtbyUzQSUzQXN3aW1taW8=" repo-name="cics-genapp-demo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
