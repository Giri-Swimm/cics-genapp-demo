---
title: Handling Insurance Policy Data Inserts (LGAPOL01) - Overview
---
# Overview

This document explains the flow of processing insurance policy requests. Incoming policy data is validated, routed, and processed to calculate premiums and make underwriting decisions for commercial policies, while non-commercial policies are rejected with appropriate status and reason.

```mermaid
flowchart TD
    node1["Validating Input Length and Preparing for Data Processing"]:::HeadingStyle --> node2["Running the Main Policy Processing Workflow"]:::HeadingStyle
    click node1 goToHeading "Validating Input Length and Preparing for Data Processing"
    click node2 goToHeading "Running the Main Policy Processing Workflow"
    node2 --> node3["Loading Configuration or Setting Defaults"]:::HeadingStyle
    click node3 goToHeading "Loading Configuration or Setting Defaults"
    node3 --> node4["Reading, Validating, and Routing Policy Records"]:::HeadingStyle
    click node4 goToHeading "Reading, Validating, and Routing Policy Records"
    node4 --> node5{"Routing Valid Records for Commercial or Non-Commercial Processing
(Routing Valid Records for Commercial or Non-Commercial Processing)"}:::HeadingStyle
    click node5 goToHeading "Routing Valid Records for Commercial or Non-Commercial Processing"
    node5 -->|"Commercial"|node6["Calculating and Applying Premiums for Commercial Policies"]:::HeadingStyle
    click node6 goToHeading "Calculating and Applying Premiums for Commercial Policies"
    node5 -->|"Non-Commercial"|node7["Rejecting Non-Commercial Policy Records"]:::HeadingStyle
    click node7 goToHeading "Rejecting Non-Commercial Policy Records"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Programs

- LGAPOL01 (<SwmPath>[base/src/lgapol01.cbl](base/src/lgapol01.cbl)</SwmPath>)
- LGAPDB01 (<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>)
- LGAPDB02
- LGAPDB03 (<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>)
- LGAPDB04 (<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>)
- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)

### Copybooks

- SQLCA
- INPUTREC2 (<SwmPath>[base/src/INPUTREC2.cpy](base/src/INPUTREC2.cpy)</SwmPath>)
- OUTPUTREC (<SwmPath>[base/src/OUTPUTREC.cpy](base/src/OUTPUTREC.cpy)</SwmPath>)
- WORKSTOR (<SwmPath>[base/src/WORKSTOR.cpy](base/src/WORKSTOR.cpy)</SwmPath>)
- LGAPACT (<SwmPath>[base/src/LGAPACT.cpy](base/src/LGAPACT.cpy)</SwmPath>)
- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)

# Where is this program used?

This program is used multiple times in the codebase as represented in the following diagram:

```mermaid
graph TD
  g9nx3("Managing Commercial Insurance Policies (LGTESTP4)") --> i0cy5("Handling Insurance Policy Data Inserts (LGAPOL01)"):::currentEntity
click g9nx3 openCode "base/src/lgtestp4.cbl:1"
pgkma("Endowment Policy Menu (LGTESTP2)") --> i0cy5("Handling Insurance Policy Data Inserts (LGAPOL01)"):::currentEntity
click pgkma openCode "base/src/lgtestp2.cbl:1"
b54dc("Motor Policy Menu (LGTESTP1)") --> i0cy5("Handling Insurance Policy Data Inserts (LGAPOL01)"):::currentEntity
click b54dc openCode "base/src/lgtestp1.cbl:1"
ez202("House Policy Menu (LGTESTP3)") --> i0cy5("Handling Insurance Policy Data Inserts (LGAPOL01)"):::currentEntity
click ez202 openCode "base/src/lgtestp3.cbl:1"
  
  
click i0cy5 openCode "base/src/lgapol01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   g9nx3("Managing Commercial Insurance Policies (LGTESTP4)") --> i0cy5("Handling Insurance Policy Data Inserts (LGAPOL01)"):::currentEntity
%% click g9nx3 openCode "<SwmPath>[base/src/lgtestp4.cbl](base/src/lgtestp4.cbl)</SwmPath>:1"
%% pgkma("Endowment Policy Menu (LGTESTP2)") --> i0cy5("Handling Insurance Policy Data Inserts (LGAPOL01)"):::currentEntity
%% click pgkma openCode "<SwmPath>[base/src/lgtestp2.cbl](base/src/lgtestp2.cbl)</SwmPath>:1"
%% b54dc("Motor Policy Menu (LGTESTP1)") --> i0cy5("Handling Insurance Policy Data Inserts (LGAPOL01)"):::currentEntity
%% click b54dc openCode "<SwmPath>[base/src/lgtestp1.cbl](base/src/lgtestp1.cbl)</SwmPath>:1"
%% ez202("House Policy Menu (LGTESTP3)") --> i0cy5("Handling Insurance Policy Data Inserts (LGAPOL01)"):::currentEntity
%% click ez202 openCode "<SwmPath>[base/src/lgtestp3.cbl](base/src/lgtestp3.cbl)</SwmPath>:1"
%%   
%%   
%% click i0cy5 openCode "<SwmPath>[base/src/lgapol01.cbl](base/src/lgapol01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1nZW5hcHAtZGVtbyUzQSUzQXN3aW1taW8=" repo-name="cics-genapp-demo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
