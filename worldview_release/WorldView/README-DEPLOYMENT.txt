WorldView static application v1.0.0

Deployment
1. Upload the CONTENTS of this WorldView folder to the document root of the password-protected site.
2. Keep the assets and data subfolders in their existing relative locations.
3. Configure the host to serve index.html as the default document.
4. Authentication must be supplied by the host. Do not add passwords to JavaScript files.
5. After deployment, open validation.html and confirm that all browser checks pass.

Student application
Open index.html through the web host. Do not open it directly as a local file because browser security rules may block JSON loading.

Data assets
The student CSV contains an anonymous WorldView identifier, two country fields, and 29 approved variables.
The browser JSON contains the numerical representations used for summaries, visualisations, and correlations.
No country contributes more than 1,000 participants.

Quality assurance
validation.html compares browser summaries and correlations with R-generated reference fixtures.
The validation page is not linked from student navigation.
