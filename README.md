
---

# **CICS Student Information & Maintenance Suite**

A mainframe-based suite of COBOL/CICS programs and JCL scripts that implements:
- A **Customer Inquiry** program (`CMINQYYY`)
- A **Customer Menu** program (`UUMENYYY`)
- A **Customer Maintenance** program (`CMMNTYYY`)

These applications illustrate key mainframe concepts, including CICS transactions, BMS map definitions, VSAM file creation and I/O, and standard mainframe job flows (JCL).

<details>
<summary><strong>Table of Contents</strong></summary>

1. [Overview](#overview)  
2. [Repository Structure](#repository-structure)  
3. [Key Components](#key-components)  
    - [Inquiry Program (Assignment 05 Part 1)](#inquiry-program-assignment-05-part-1)  
    - [Menu Program (Assignment 05 Part 2)](#menu-program-assignment-05-part-2)  
    - [Maintenance Program (Assignment-05-Part-3)](#maintenance-program-assignment-05-part-3)  
4. [Installation & Setup](#installation--setup)  
    - [1. VSAM Creation](#1-vsam-creation)  
    - [2. BMS Generation](#2-bms-generation)  
    - [3. Program Compilation](#3-program-compilation)  
    - [4. CICS Definitions](#4-cics-definitions)  
5. [Sample Screens & Execution Flow](#sample-screens--execution-flow)  
6. [Contributing](#contributing)  
7. [License](#license)  

</details>

---

## **Overview**
This project contains three interrelated COBOL/CICS applications that manage “customer” data on an IBM Mainframe. Each application uses a shared VSAM file (`CMFYYY`) as the customer master data source, and each has its own BMS mapset, transaction ID, and CICS definitions.  

- **Inquiry Program**: Allows inquiry of customer information by customer ID.  
- **Menu Program**: Simple CICS menu to route you to Inquiry, Maintenance, or display an error if an option is not available.  
- **Maintenance Program**: Allows adding, changing, and deleting customer information in the VSAM file.

---

## **Repository Structure**

(names can vary depending on your exact user ID and file naming):

```
CICS-Student-Info-Maint-Suite/
│
├── 📂 inquiry_program/             # Files related to Customer Inquiry
│   ├── CMINQF2A.cbl               # COBOL source for the Customer Inquiry program
│   ├── CMINQF2A.jcl               # JCL script to compile/link CMINQF2A
│   ├── CMINQF2A.pdf               # PDF of entire job summary (compile result/log)
│   ├── INQSF2A.bms                # BMS map definition source for Inquiry screen
│   ├── INQSF2A.cbl                # Generated COBOL copybook from BMS (if used separately)
│   ├── INQSF2A.jcl                # JCL script to assemble/link the BMS map
│   ├── INQSF2A.pdf                # PDF of entire job summary for the BMS generate
│   ├── CMFLOF2A.cbl               # COBOL source for loading the VSAM file (data load pgm)
│   ├── CMFLOF2A.jcl               # JCL script to compile/link or execute the load pgm
│   ├── CMFLOF2A.pdf               # PDF of entire job summary (compile/execute log)
│   ├── CMFCLF2A.jcl               # JCL to create the VSAM cluster (CMF file)
│   ├── CMFCLF2A.pdf               # Job summary PDF of VSAM cluster creation
│   ├── CMFFLF2A.jcl               # JCL to create a flat file from the VSAM
│   ├── CMFFLF2A.pdf               # Job summary PDF of flat file creation
│   ├── CMFLEF2A.jcl               # JCL to compile/link the data load program
│   ├── CMFLEF2A.pdf               # Job summary PDF for that compile/link
│   ├── DFHZITCL.jcl               # Subroutine procedure (CICS compile/link) if needed
│   ├── KC03F2A.DCMAFD02...P1.CICS.CMFF2A.FLAT  # Flat file output from VSAM
│   ├── (Screenshots1)              # Various screenshots: 
│   │   ├── PART 1 screenshot of CEDA DISPLAY.png
│   │   ├── SCREEN Initial screen...ing Transaction ID IF2A.png
│   │   ├── Screen with ‘Custom...fter entering ‘777777’.png
│   │   ├── Screen with error mes...ustomer Number field.png
│   │   ├── Screen with valid data after entering ‘123456’.png
│   │   ├── VSAM CLOSE.png
│   │   └── VSAM OPEN.png
│   └── ...
│
├── 📂 menu_program/             # Files related to Menu program
│   ├── UUMENF2A.cbl               # COBOL source for the Menu program
│   ├── UUMENF2A.jcl               # JCL script to compile/link UUMENF2A
│   ├── MENSF2A.bms                # BMS map definition source for Menu screen
│   ├── MENSF2A.cbl                # Generated COBOL copybook from BMS (if separated)
│   ├── MENSBF2A.jcl               # JCL script to assemble/link the BMS map
│   ├── COBOL entire job summary.pdf        # Consolidated compile logs
│   ├── BMS entire job summary.pdf .pdf     # BMS generate logs
│   ├── CMINQF2A entire job summary.pdf     # Possibly referencing Inquiry compile log
│   ├── Inquiry Screen before...nsfer to Menu program.png
│   ├── Screen with ‘1’ typed...nsfer to Inquiry program.png
│   ├── Screen with ‘Program...after entering ‘2’ or ‘3’.png
│   ├── Screen with error message after entering ‘4’.png
│   ├── Screen with error mes...lanks in selection field.png
│   ├── screenshot of CEDA DISPLAY.png
│   ├── Screenshot OF CEMT...MFF2A) CLOSE CICS.png
│   ├── Screenshot OF CEMT...T FILE(CMFF2A) OPEN.png
│   └── ...
│
├── 📂 maintenance_program/             # Files related to Maintenance program
│   ├── CMMNTF2A.cbl               # COBOL source for the Maintenance program
│   ├── CMMNTF2A.jcl               # JCL script to compile/link CMMNTF2A
│   ├── MNTSF2A.bms                # BMS map definition source for Maintenance screens
│   ├── MNTSF2A.cbl                # Generated COBOL copybook from BMS (if separated)
│   ├── MNTSBF2A.jcl               # JCL script to assemble/link the Maintenance BMS
│   ├── CMMNTF2A entire job summary.pdf     # Job log for the Maintenance compile
│   ├── MNTSBF2A BMS entire job summary.pdf # Job log for the BMS map generate
│   ├── Screen for Add with error.png
│   ├── Screen for Add with message.png
│   ├── Screen for Change with error.png
│   ├── Screen for Delete with error.png
│   ├── Screen for Delete with message.png
│   ├── Screenshot of three...ams, three Transactions.png
│   ├── Screenshot CEMT SE...MFF2A) CLOSE CICS.png
│   ├── Screenshot CEMT SET FILE(CMFF2A) OPEN.png
│   └── ...
│
├── README.md                     # project overview and setup guide
└── LICENSE                       # License file
```

Within each `related folder#` , you will find:
- **COBOL source files** (`.cbl`)  
- **JCL files** (`.jcl`) for compilation, linking, loading data, etc.  
- **BMS source** (`.bms`) for mapsets  
- **PDF or text job summaries** (e.g., `CMINQF2A entire job summary.pdf`)  
- **Screenshots** of CICS screens, open/close VSAM confirmations, etc.

---

## **Key Components**

### Inquiry Program (Part 1)
- **Program Name**: `CMINQYYY`  
- **Mapset**: `INQSYYY` / Map: `INQMYYY`  
- **Transaction ID**: `IYYY`  
- **Functions**:  
  - Reads VSAM file `CMFYYY` to display customer info.  
  - PF3/PF12 logic for returning to a menu or ending execution.

### Menu Program (Part 2)
- **Program Name**: `UUMENYYY`  
- **Mapset**: `MENSYYY` / Map: `MENMYYY`  
- **Transaction ID**: `UYYY`  
- **Functions**:  
  - Presents a menu to route to Inquiry, Maintenance, or show “Program not available” messages.  
  - PF3/PF12 logic to handle return states.

### Maintenance Program (Part 3)
- **Program Name**: `CMMNTYYY`  
- **Mapset**: `MNTSYYY` / Maps: `MNT1YYY`, `MNT2YYY`  
- **Transaction ID**: `VYYY`  
- **Functions**:  
  - Opens and closes VSAM file `CMFYYY` as needed.  
  - Add, Change, or Delete customer records.  
  - PF3/PF12 to return to the menu program.

---

## **Installation & Setup**

Below is a short overview of how you set up this application on a z/OS mainframe.

### 1. VSAM Creation
1. Allocate the VSAM cluster using `CMFCLYYY.jcl` (e.g., `DEFINE CLUSTER ...`).  
2. Compile and execute the VSAM initialization and load programs to populate test data (e.g., `CMFINYYY.jcl`, `CMFIEYYY.jcl`, `CMFLEYYY.jcl`, `CMFLOYYY.jcl`).

### 2. BMS Generation
- Use the various `*SBYYY.jcl` members (e.g., `INQSBYYY.jcl`, `MENSBYYY.jcl`, `MNTSBYYY.jcl`) to assemble and link BMS mapsets into your CICS LOADLIB.

### 3. Program Compilation
- Compile each COBOL program using the JCL provided (`CMINQYYY.jcl`, `UUMENYYY.jcl`, `CMMNTYYY.jcl`).  
- Ensure RETURN CODE is 0 or 4 (warnings are typically ignorable).

### 4. CICS Definitions
1. **CEDA DEFINE MAPSET(...), PROGRAM(...), TRANSACTION(...)** commands for each application.  
2. **CEDA DEFINE FILE** for `CMFYYY`, linking to your newly created VSAM dataset.  
3. **CEMT SET FILE(CMFYYY) OPEN** (or CLOSE) to verify your file is accessible in CICS.  
4. **CEMT SET PROGRAM(...) NEWCOPY** when you recompile changes.

---

## **Sample Screens & Execution Flow**

Below are embedded screenshots demonstrating how the application works in CICS. (These references assume you’ve placed the images in the relevant `assignment5_p#` folder.)

```

```
## **Inquiry Program Screens**
- **PART 1: CEDA DISPLAY**  
  ```markdown
  ![CEDA DISPLAY](./screenshots1/PART%201%20screenshot%20of%20CEDA%20DISPLAY%20.png)
  ```

- **Initial Screen (`IF2A`)**  
  ```markdown
  ![Inquiry Initial Screen](./screenshots1/SCREEN%20Initial%20screen%20after%20entering%20Transaction%20ID%20IF2A.png)
  ```

- **“Customer does not exist”**  
  ```markdown
  ![Inquiry Invalid](./screenshots1/Screen%20with%20‘Customer%20does%20not%20exist’%20message%20after%20entering%20‘777777’%20.png)
  ```

- **Error Message on Blank Input**  
  ```markdown
  ![Inquiry Error Blank](./screenshots1/Screen%20with%20error%20message%20after%20entering%20blanks%20in%20Customer%20Number%20field.png)
  ```

- **Valid Data after entering `123456`**  
  ```markdown
  ![Inquiry Valid Data](./screenshots1/Screen%20with%20valid%20data%20after%20entering%20‘123456’%20.png)
  ```

- **VSAM CLOSE**  
  ```markdown
  ![VSAM CLOSE](./screenshots1/VSAM%20CLOSE.png)
  ```

- **VSAM OPEN**  
  ```markdown
  ![VSAM OPEN](./screenshots1/VSAM%20OPEN.png)
  ```
```

```
## **Menu Program Screens (`UYYY`)**

- **Menu Initial Screen**  
  - **Description**: Displays the initial menu before transferring to another program.
  ```markdown
  ![Menu Initial Screen](./screenshots2/Inquiry%20Screen%20before%20pressing%20F3%20or%20F12%20to%20transfer%20to%20Menu%20program.png)
  ```

- **Option 1 → Transfer to Inquiry Program**  
  - **Description**: Shows the screen when selecting `1` to transfer to the Inquiry Program.
  ```markdown
  ![Menu Option 1 to Inquiry](./screenshots2/Screen%20with%20‘1’%20typed%20before%20pressing%20Enter%20to%20transfer%20to%20Inquiry%20program.png)
  ```

- **Option 2 → "Program Not Available"**  
  - **Description**: Displays a message when selecting `2` or `3` before the Maintenance Program is set up.
  ```markdown
  ![Menu Option 2 Error](./screenshots2/Screen%20with%20‘Program%20not%20available’%20message%20after%20entering%20‘2’%20or%20‘3.png)
  ```

- **Invalid Input Error**  
  - **Description**: Error screen when an invalid input (such as `4`) is entered.
  ```markdown
  ![Menu Invalid Input Error](./screenshots2/Screen%20with%20error%20message%20after%20entering%20‘4’%20.png)
  ```

- **Blank Input Error in Selection Field**  
  - **Description**: Displays an error message when a blank selection is entered.
  ```markdown
  ![Menu Blank Input Error](./screenshots2/Screen%20with%20error%20message%20after%20entering%20blanks%20in%20selection%20field.png)
  ```

- **CEDA DISPLAY Screenshot**  
  - **Description**: Shows the CEDA DISPLAY configuration for the system.
  ```markdown
  ![CEDA DISPLAY](./screenshots2/screenshot%20of%20CEDA%20DISPLAY%20.png)
  ```

- **CICS File Closed Status**  
  - **Description**: Displays the CICS file status when `CMFF2A` is closed.
  ```markdown
  ![CICS File Closed](./screenshots2/Screenshot%20OF%20CEMT%20SET%20FILE(CMFF2A)%20CLOSE%20CICS%20.png)
  ```

- **CICS File Open Status**  
  - **Description**: Displays the CICS file status when `CMFF2A` is opened.
  ```markdown
  ![CICS File Open](./screenshots2/Screenshot%20OF%20CEMT%20SET%20FILE(CMFF2A)%20OPEN.png)
  ```
```

```
3.## **Maintenance Program Screens (`VYYY`)**

- **Add Operation - Error (No Customer Number Provided)**  
  - **Description**: Displays an error when attempting to add a record without providing a customer number.
  ```markdown
  ![Maint Add Error](./screenshots3/Screen%20for%20Add%20with%20message%20.png)
  ```

- **Add Operation - Success**  
  - **Description**: Displays a success message when a record is successfully added.
  ```markdown
  ![Maint Add Success](./screenshots3/Screen%20for%20Add%20with%20message%20.png)
  ```

- **Change Operation - Error (Customer Does Not Exist)**  
  - **Description**: Displays an error when attempting to change a non-existent customer record.
  ```markdown
  ![Maint Change Error](./screenshots3/Screen%20for%20Change%20with%20error%20.png)
  ```

- **Delete Operation - Error**  
  - **Description**: Displays an error when attempting to delete a record with an issue.
  ```markdown
  ![Maint Delete Error](./screenshots3/Screen%20for%20Delete%20with%20error%20.png)
  ```

- **Delete Operation - Success**  
  - **Description**: Displays a success message when a record is successfully deleted.
  ```markdown
  ![Maint Delete Success](./screenshots3/Screen%20for%20Delete%20with%20message%20.png)
  ```

- **System Overview - Mapsets, Programs, and Transactions**  
  - **Description**: Shows an overview of the system with three mapsets, three programs, and three transactions.
  ```markdown
  ![System Overview](./screenshots3/Screenshot%20%20of%20three%20Mapsets,%20three%20Programs,%20three%20Transactions%20.png)
  ```

- **CICS File Closed Status**  
  - **Description**: Displays the CICS file status when `CMFF2A` is closed.
  ```markdown
  ![CICS File Closed](./screenshots3/Screenshot%20CEMT%20SET%20FILE(CMFF2A)%20CLOSE%20CICS%20%20.png)
  ```

- **CICS File Open Status**  
  - **Description**: Displays the CICS file status when `CMFF2A` is opened.
  ```markdown
  ![CICS File Open](./screenshots3/Screenshot%20CEMT%20SET%20FILE(CMFF2A)%20OPEN.png)
  ```
```
```
Use PF3/PF12 on either Inquiry or Maint screens to return seamlessly to the main Menu.

---

## **License**
This project is provided as-is for educational and demonstration purposes.  
You may add your own license (e.g., MIT) if you intend to open-source it more broadly.

---
## 👨‍💻 **About the Developer**  

📌 **Talent Nyota**  
📌 **GitHub**: [devtalent2030](https://github.com/devtalent2030)  
📌 **Expertise**: COBOL | JCL | CICS | VSAM | Mainframe Development  
