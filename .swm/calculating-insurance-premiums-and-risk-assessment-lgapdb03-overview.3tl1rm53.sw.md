---
title: Calculating Insurance Premiums and Risk Assessment (LGAPDB03) - Overview
---
# Overview

This document describes the flow for processing insurance policy applications, covering risk assessment, eligibility determination, and premium calculation. The process ensures risk factors are available and applies business rules to produce a policy verdict and premium.

## Dependencies

### Program

- LGAPDB03 (<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>)

### Copybook

- SQLCA

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  z61ql("Enhanced Policy Premium Calculation (LGAPDB01)") --> gqktc("Calculating Insurance Premiums and Risk Assessment (LGAPDB03)"):::currentEntity
click z61ql openCode "base/src/LGAPDB01.cbl:1"
  
  
click gqktc openCode "base/src/LGAPDB03.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   z61ql("Enhanced Policy Premium Calculation (LGAPDB01)") --> gqktc("Calculating Insurance Premiums and Risk Assessment (LGAPDB03)"):::currentEntity
%% click z61ql openCode "<SwmPath>[base/src/LGAPDB01.cbl](base/src/LGAPDB01.cbl)</SwmPath>:1"
%%   
%%   
%% click gqktc openCode "<SwmPath>[base/src/LGAPDB03.cbl](base/src/LGAPDB03.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1nZW5hcHAtZGVtbyUzQSUzQXN3aW1taW8=" repo-name="cics-genapp-demo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
