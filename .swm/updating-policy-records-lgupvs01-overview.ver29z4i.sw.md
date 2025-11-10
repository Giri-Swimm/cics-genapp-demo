---
title: Updating Policy Records (LGUPVS01) - Overview
---
# Overview

This document describes the flow for updating insurance policy records. The process interprets incoming requests, updates relevant fields, and ensures errors are logged for traceability.

## Dependencies

### Programs

- LGUPVS01 (<SwmPath>[base/src/lgupvs01.cbl](base/src/lgupvs01.cbl)</SwmPath>)
- LGSTSQ (<SwmPath>[base/src/lgstsq.cbl](base/src/lgstsq.cbl)</SwmPath>)

### Copybook

- LGCMAREA (<SwmPath>[base/src/lgcmarea.cpy](base/src/lgcmarea.cpy)</SwmPath>)

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  9xu2l("Updating Policy Details (LGUPDB01)") --> 8d3to("Updating Policy Records (LGUPVS01)"):::currentEntity
click 9xu2l openCode "base/src/lgupdb01.cbl:1"
  
  
click 8d3to openCode "base/src/lgupvs01.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   9xu2l("Updating Policy Details (LGUPDB01)") --> 8d3to("Updating Policy Records (LGUPVS01)"):::currentEntity
%% click 9xu2l openCode "<SwmPath>[base/src/lgupdb01.cbl](base/src/lgupdb01.cbl)</SwmPath>:1"
%%   
%%   
%% click 8d3to openCode "<SwmPath>[base/src/lgupvs01.cbl](base/src/lgupvs01.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1nZW5hcHAtZGVtbyUzQSUzQXN3aW1taW8=" repo-name="cics-genapp-demo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
