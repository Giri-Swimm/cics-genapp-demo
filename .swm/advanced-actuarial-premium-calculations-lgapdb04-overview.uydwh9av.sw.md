---
title: Advanced Actuarial Premium Calculations (LGAPDB04) - Overview
---
# Overview

This document describes how insurance policy premiums are calculated by adjusting exposures, applying risk and schedule modifiers, summing peril premiums, applying discounts, and finalizing the premium with taxes and regulatory caps.

```mermaid
flowchart TD
    node1["Exposure and Insured Value Setup"]:::HeadingStyle --> node2["Experience Modifier Calculation"]:::HeadingStyle
    click node1 goToHeading "Exposure and Insured Value Setup"
    click node2 goToHeading "Experience Modifier Calculation"
    node2 --> node3["Schedule Modifier Logic"]:::HeadingStyle
    click node3 goToHeading "Schedule Modifier Logic"
    node3 --> node4["Base Premium Calculation"]:::HeadingStyle
    click node4 goToHeading "Base Premium Calculation"
    node4 --> node5["Discount Calculation"]:::HeadingStyle
    click node5 goToHeading "Discount Calculation"
    node5 --> node6["Tax and Final Premium Calculation"]:::HeadingStyle
    click node6 goToHeading "Tax and Final Premium Calculation"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Program

- LGAPDB04 (<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>)

### Copybook

- SQLCA

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  nl769("Enhanced Policy Premium Calculation (LGAPDB01)") --> yran9("Advanced Actuarial Premium Calculations (LGAPDB04)"):::currentEntity
click nl769 openCode "base/src/LGAPDB01.cbl:1"
  
  
click yran9 openCode "base/src/LGAPDB04.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   nl769("Enhanced Policy Premium Calculation (LGAPDB01)") --> yran9("Advanced Actuarial Premium Calculations (LGAPDB04)"):::currentEntity
%% click nl769 openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%   
%%   
%% click yran9 openCode "<SwmPath>[base/src/LGAPDB04.cbl](base/src/LGAPDB04.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1nZW5hcHAtZGVtbyUzQSUzQXN3aW1taW8=" repo-name="cics-genapp-demo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
