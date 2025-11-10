---
title: General Insurance Motor Policy Menu
---
The Motor Policy Menu screen allows users to inquire, add, delete, or update motor insurance policies by entering or viewing policy and vehicle details. It serves as the main entry point for managing motor policy records in the application.

## Screen Preview

```
SSP1        General Insurance Motor Policy Menu  

        1. Policy Inquiry 
        2. Policy Add     
        3. Policy Delete  
        4. Policy Update  

                              Policy Number __________
                              Cust Number  __________
                              Issue date   __________ (yyyy-mm-dd)
                              Expiry date  __________ (yyyy-mm-dd)
                              Car Make     ____________________
                              Car Model    ____________________
                              Car Value    ______
                              Registration ________
                              Car Colour   ________
                              CC           ________
                              Manufacture Date __________ (yyyy-mm-dd)
                              No. of Accidents ______
                              Policy Premium    ______

        Select Option _


        [                                        ]

ENTER=Continue  PF3=Exit  CLEAR=Clear
```

## Fields

### Policy Number (ENP1PNO)

- Length: 10 digits
- Input field, right-justified, zero-filled
- Used for identifying the motor policy
- No explicit validation in the provided code, but likely must be numeric

### Cust Number (ENP1CNO)

- Length: 10 digits
- Input field, right-justified, zero-filled
- Used for identifying the customer
- No explicit validation in the provided code, but likely must be numeric

### Issue date (ENP1IDA)

- Length: 10 characters
- Input field
- Expected format: yyyy-mm-dd
- No explicit validation in the provided code

### Expiry date (ENP1EDA)

- Length: 10 characters
- Input field
- Expected format: yyyy-mm-dd
- No explicit validation in the provided code

### Car Make (ENP1CMK)

- Length: 20 characters
- Input field
- No explicit validation in the provided code

### Car Model (ENP1CMO)

- Length: 20 characters
- Input field
- No explicit validation in the provided code

### Car Value (ENP1VAL)

- Length: 6 digits
- Input field, right-justified, zero-filled
- No explicit validation in the provided code, but likely must be numeric

### Registration (ENP1REG)

- Length: 7 characters
- Input field
- No explicit validation in the provided code

### Car Colour (ENP1COL)

- Length: 8 characters
- Input field
- No explicit validation in the provided code

### CC (ENP1CC)

- Length: 8 digits
- Input field, right-justified, zero-filled
- No explicit validation in the provided code, but likely must be numeric

### Manufacture Date (ENP1MAN)

- Length: 10 characters
- Input field
- Expected format: yyyy-mm-dd
- No explicit validation in the provided code

### No. of Accidents (ENP1ACC)

- Length: 6 digits
- Input field, right-justified, zero-filled
- No explicit validation in the provided code, but likely must be numeric

### Policy Premium (ENP1PRE)

- Length: 6 digits
- Input field, right-justified, zero-filled
- No explicit validation in the provided code, but likely must be numeric

### Select Option (ENP1OPT)

- Length: 1 digit
- Input field, numeric only
- Must be entered (MUSTENTER)
- Used to select menu option (1-4)
- If invalid, error message is shown and cursor is placed here

### Error/Status Message (ERP1FLD)

- Length: 40 characters
- Output only (protected)
- Used to display error or status messages to the user
- Populated by the COBOL program based on processing results

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1nZW5hcHAtZGVtbyUzQSUzQXN3aW1taW8=" repo-name="cics-genapp-demo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
