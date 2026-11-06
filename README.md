
--------------------------------------------------------------------------------
USER GUIDE: SRFSight & SRFQRSight
--------------------------------------------------------------------------------

This document explains how to use the two R Shiny applications:
1. SRFSight  - Balanced & Conflict-Free Evaluator Allocation Engine
2. SRFQRSight - Verification QR Code Generator

--------------------------------------------------------------------------------
PART 1: HOW TO USE SRFSight (Evaluator Allocation Engine)
--------------------------------------------------------------------------------

OVERVIEW:
SRFSight automates the fair, balanced distribution of research proposals to 
evaluators while honoring institutional Conflict of Interest (COI) policies.

STEP-BY-STEP INSTRUCTIONS:

1. Download Data Templates
   - Open the application and stay on the "Randomization" tab.
   - Click "Evaluator Template (.xlsx)" to download the reviewer sheet.
   - Click "Proposal Template (.xlsx)" to download the proposal sheet.

2. Populate Your Data
   - In the Evaluators sheet, ensure two columns exist:
       * evaluator_name (must be unique per person)
       * department
   - In the Proposals sheet, ensure five columns exist:
       * proposal_id
       * guide
       * guide_dept
       * co_guide
       * co_guide_dept
   - Save your files as .xlsx or .csv.

3. Upload the Files
   - Under section "2. Upload Data", use the file browse buttons to upload:
       * Your completed Evaluators file.
       * Your completed Proposals file.

4. Configure Assignment Parameters
   - Evaluators per Proposal: Enter the required number of reviewers (e.g., 3).
   - Conflict of Interest (COI) Rule:
       * "Randomize only": No restrictions applied.
       * "Exclude guide & co-guide": Mentors are barred from their own proposals.
       * "Exclude guide, co-guide, & departmental faculty": Excludes mentors 
         plus any faculty member sharing the proposal's home department.
   - Random Seed: Set any integer (e.g., 101) for auditability. Using the same 
     seed with identical data produces the exact same assignment.

5. Generate & Export Results
   - Click the "Generate Allocation" button.
   - Review the on-screen allocation table displaying total assigned workloads.
   - Click "Download Allocation (.xlsx)" to save the finalized schedule.


--------------------------------------------------------------------------------
PART 2: HOW TO USE SRFQRSight (QR Code Generator)
--------------------------------------------------------------------------------

OVERVIEW:
SRFQRSight creates scannable, high error-correction verification QR codes 
containing candidate and guide metadata, suitable for certificates or records.

STEP-BY-STEP INSTRUCTIONS:

1. Enter Candidate Details
   - In the left sidebar panel, fill in:
       * SRF Number (e.g., SRF-2024-001)
       * Student Name
       * Guide Name
       * Co-guide Name
       * Completion Date (selected via date picker)

2. Customize QR Appearance
   - Background Color:
       * Choose "White" for standard printing.
       * Choose "Transparent" for seamless inclusion on pre-printed certificates.
   - Upload Center Logo (Optional):
       * Browse and select an institutional logo (.png or .jpeg).
       * The app automatically scales and centers the logo inside the code.

3. Generate QR Code
   - Click the "Generate QR Code" button.
   - Check the preview box on the right to inspect the QR code and the 
     "Entered Details" verification table.

4. Download the Image
   - Click "Download QR Code (PNG)".
   - The file will be saved as "SRFQRSight_<SRF_Number>.png".

--------------------------------------------------------------------------------
CREDITS & ATTRIBUTION:
Prepared by: Dr. Himel Mondal
Department of Physiology, AIIMS Deoghar, Jharkhand, India
Contact: himel.physiology@aiimsdeoghar.edu.in
--------------------------------------------------------------------------------
