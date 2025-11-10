---
title: Calculating Risk Scores (RISKPROG) - Overview
---
# Program Overview

This document describes the flow for processing insurance policy records (LGARSK01). The program reads, validates, and scores each policy, assigning a risk category and producing output for further insurance processing.

```mermaid
flowchart TD
    node1["Main Loop and File Handling"] --> node2["Record Processing and Validation"]
    click node1 goToHeading "Main Loop and File Handling"
    click node2 goToHeading "Record Processing and Validation"
    node2 --> node3{"Is record valid?"}
    node3 -->|"Yes"| node4["Risk Score Calculation"]
    node3 -->|"No"| node5["Output Generation and Record Finalization"]
    click node4 goToHeading "Risk Score Calculation"
    click node5 goToHeading "Output Generation and Record Finalization"
    node4 --> node5
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1nZW5hcHAtZGVtbyUzQSUzQXN3aW1taW8=" repo-name="cics-genapp-demo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
