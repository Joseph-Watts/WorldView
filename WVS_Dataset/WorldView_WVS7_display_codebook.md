# WorldView WVS Wave 7 Codebook

This codebook describes the processed variables displayed in the WorldView Shiny app. Original negative missing and non-response codes are simplified to `NA`. Where the setup script applies a custom recode, the displayed values below are the processed values used by the app.

Click a section title to expand it and view the variables in that section.

<style>
.codebook-section { margin: 0.75rem 0; padding: 0.6rem 0.8rem; border: 1px solid #ddd; border-radius: 6px; background: #fafafa; }
.codebook-section summary { cursor: pointer; font-size: 1.15rem; }
.codebook-section[open] summary { margin-bottom: 0.75rem; }
.codebook-variable { margin: 1rem 0 1.25rem 0; padding-bottom: 0.75rem; border-bottom: 1px solid #e5e5e5; }
.codebook-variable h3 { margin-top: 0; }
.codebook-variable ul { margin-top: 0.25rem; }
</style>

<details class="codebook-section">
<summary><strong>RELIGIOUS VALUES</strong> <span class="text-muted">(4 variables)</span></summary>

<div class="codebook-variable">
<h3>Q165: Belief in God</h3>
<p><strong>Question:</strong> In which of the following things do you believe, if you believe in any? - God</p>
<p><strong>Processed data type:</strong> numeric</p>
<p><strong>Processed values:</strong></p>
<ul>
<li>0 = No</li>
<li>1 = Yes</li>
</ul>
<p><strong>Processing note:</strong> Custom recode in setup script; all unmatched responses and original negative missing codes are set to NA.</p>
</div>

<div class="codebook-variable">
<h3>Q166: Belief in life after death</h3>
<p><strong>Question:</strong> In which of the following things do you believe, if you believe in any? - Life after death</p>
<p><strong>Processed data type:</strong> numeric</p>
<p><strong>Processed values:</strong></p>
<ul>
<li>0 = No</li>
<li>1 = Yes</li>
</ul>
<p><strong>Processing note:</strong> Custom recode in setup script; all unmatched responses and original negative missing codes are set to NA.</p>
</div>

<div class="codebook-variable">
<h3>Q167: Belief in Hell</h3>
<p><strong>Question:</strong> In which of the following things do you believe, if you believe in any? - Hell</p>
<p><strong>Processed data type:</strong> numeric</p>
<p><strong>Processed values:</strong></p>
<ul>
<li>0 = No</li>
<li>1 = Yes</li>
</ul>
<p><strong>Processing note:</strong> Custom recode in setup script; all unmatched responses and original negative missing codes are set to NA.</p>
</div>

<div class="codebook-variable">
<h3>Q168: Belief in Heaven</h3>
<p><strong>Question:</strong> In which of the following things do you believe, if you believe in any? - Heaven</p>
<p><strong>Processed data type:</strong> numeric</p>
<p><strong>Processed values:</strong></p>
<ul>
<li>0 = No</li>
<li>1 = Yes</li>
</ul>
<p><strong>Processing note:</strong> Custom recode in setup script; all unmatched responses and original negative missing codes are set to NA.</p>
</div>

</details>

<details class="codebook-section">
<summary><strong>ETHICAL VALUES AND NORMS</strong> <span class="text-muted">(19 variables)</span></summary>

<div class="codebook-variable">
<h3>Q177: Justification for claiming government benefits not entitled to</h3>
<p><strong>Question:</strong> Please tell me for each of the following statements whether you think it can always be justified, never be justified, or something in between, using this card. - Claiming government benefits to which you are not entitled</p>
<p><strong>Processed data type:</strong> integer/numeric</p>
<p><strong>Processed values:</strong></p>
<ul>
<li>1 = Never justifiable</li>
<li>2 = 2</li>
<li>3 = 3</li>
<li>4 = 4</li>
<li>5 = 5</li>
<li>6 = 6</li>
<li>7 = 7</li>
<li>8 = 8</li>
<li>9 = 9</li>
<li>10 = Always justifiable</li>
</ul>
<p><strong>Processing note:</strong> Original numeric values retained; all values below 0 are set to NA.</p>
</div>

<div class="codebook-variable">
<h3>Q178: Justification for avoiding a fare on public transport</h3>
<p><strong>Question:</strong> Please tell me for each of the following statements whether you think it can always be justified, never be justified, or something in between, using this card. - Avoiding a fare on public transport</p>
<p><strong>Processed data type:</strong> integer/numeric</p>
<p><strong>Processed values:</strong></p>
<ul>
<li>1 = Never justifiable</li>
<li>2 = 2</li>
<li>3 = 3</li>
<li>4 = 4</li>
<li>5 = 5</li>
<li>6 = 6</li>
<li>7 = 7</li>
<li>8 = 8</li>
<li>9 = 9</li>
<li>10 = Always justifiable</li>
</ul>
<p><strong>Processing note:</strong> Original numeric values retained; all values below 0 are set to NA.</p>
</div>

<div class="codebook-variable">
<h3>Q179: Justification for stealing property</h3>
<p><strong>Question:</strong> Please tell me for each of the following statements whether you think it can always be justified, never be justified, or something in between, using this card. Stealing property</p>
<p><strong>Processed data type:</strong> integer/numeric</p>
<p><strong>Processed values:</strong></p>
<ul>
<li>1 = Never justifiable</li>
<li>2 = 2</li>
<li>3 = 3</li>
<li>4 = 4</li>
<li>5 = 5</li>
<li>6 = 6</li>
<li>7 = 7</li>
<li>8 = 8</li>
<li>9 = 9</li>
<li>10 = Always justifiable</li>
</ul>
<p><strong>Processing note:</strong> Original numeric values retained; all values below 0 are set to NA.</p>
</div>

<div class="codebook-variable">
<h3>Q180: Justification for cheating on taxes</h3>
<p><strong>Question:</strong> Please tell me for each of the following statements whether you think it can always be justified, never be justified, or something in between, using this card. Cheating on taxes if you have a chance</p>
<p><strong>Processed data type:</strong> integer/numeric</p>
<p><strong>Processed values:</strong></p>
<ul>
<li>1 = Never justifiable</li>
<li>2 = 2</li>
<li>3 = 3</li>
<li>4 = 4</li>
<li>5 = 5</li>
<li>6 = 6</li>
<li>7 = 7</li>
<li>8 = 8</li>
<li>9 = 9</li>
<li>10 = Always justifiable</li>
</ul>
<p><strong>Processing note:</strong> Original numeric values retained; all values below 0 are set to NA.</p>
</div>

<div class="codebook-variable">
<h3>Q181: Justification for accepting a bribe in the course of duties</h3>
<p><strong>Question:</strong> Please tell me for each of the following statements whether you think it can always be justified, never be justified, or something in between, using this card. Someone accepting a bribe in the course of their duties</p>
<p><strong>Processed data type:</strong> integer/numeric</p>
<p><strong>Processed values:</strong></p>
<ul>
<li>1 = Never justifiable</li>
<li>2 = 2</li>
<li>3 = 3</li>
<li>4 = 4</li>
<li>5 = 5</li>
<li>6 = 6</li>
<li>7 = 7</li>
<li>8 = 8</li>
<li>9 = 9</li>
<li>10 = Always justifiable</li>
</ul>
<p><strong>Processing note:</strong> Original numeric values retained; all values below 0 are set to NA.</p>
</div>

<div class="codebook-variable">
<h3>Q182: Justification for homosexuality</h3>
<p><strong>Question:</strong> Please tell me for each of the following statements whether you think it can always be justified, never be justified, or something in between, using this card. Homosexuality</p>
<p><strong>Processed data type:</strong> integer/numeric</p>
<p><strong>Processed values:</strong></p>
<ul>
<li>1 = Never justifiable</li>
<li>2 = 2</li>
<li>3 = 3</li>
<li>4 = 4</li>
<li>5 = 5</li>
<li>6 = 6</li>
<li>7 = 7</li>
<li>8 = 8</li>
<li>9 = 9</li>
<li>10 = Always justifiable</li>
</ul>
<p><strong>Processing note:</strong> Original numeric values retained; all values below 0 are set to NA.</p>
</div>

<div class="codebook-variable">
<h3>Q183: Justification for prostitution</h3>
<p><strong>Question:</strong> Please tell me for each of the following statements whether you think it can always be justified, never be justified, or something in between, using this card. Prostitution</p>
<p><strong>Processed data type:</strong> integer/numeric</p>
<p><strong>Processed values:</strong></p>
<ul>
<li>1 = Never justifiable</li>
<li>2 = 2</li>
<li>3 = 3</li>
<li>4 = 4</li>
<li>5 = 5</li>
<li>6 = 6</li>
<li>7 = 7</li>
<li>8 = 8</li>
<li>9 = 9</li>
<li>10 = Always justifiable</li>
</ul>
<p><strong>Processing note:</strong> Original numeric values retained; all values below 0 are set to NA.</p>
</div>

<div class="codebook-variable">
<h3>Q184: Justification for abortion</h3>
<p><strong>Question:</strong> Please tell me for each of the following statements whether you think it can always be justified, never be justified, or something in between, using this card. Abortion</p>
<p><strong>Processed data type:</strong> integer/numeric</p>
<p><strong>Processed values:</strong></p>
<ul>
<li>1 = Never justifiable</li>
<li>2 = 2</li>
<li>3 = 3</li>
<li>4 = 4</li>
<li>5 = 5</li>
<li>6 = 6</li>
<li>7 = 7</li>
<li>8 = 8</li>
<li>9 = 9</li>
<li>10 = Always justifiable</li>
</ul>
<p><strong>Processing note:</strong> Original numeric values retained; all values below 0 are set to NA.</p>
</div>

<div class="codebook-variable">
<h3>Q185: Justification for divorce</h3>
<p><strong>Question:</strong> Please tell me for each of the following statements whether you think it can always be justified, never be justified, or something in between, using this card. Divorce</p>
<p><strong>Processed data type:</strong> integer/numeric</p>
<p><strong>Processed values:</strong></p>
<ul>
<li>1 = Never justifiable</li>
<li>2 = 2</li>
<li>3 = 3</li>
<li>4 = 4</li>
<li>5 = 5</li>
<li>6 = 6</li>
<li>7 = 7</li>
<li>8 = 8</li>
<li>9 = 9</li>
<li>10 = Always justifiable</li>
</ul>
<p><strong>Processing note:</strong> Original numeric values retained; all values below 0 are set to NA.</p>
</div>

<div class="codebook-variable">
<h3>Q186: Justification for sex before marriage</h3>
<p><strong>Question:</strong> Please tell me for each of the following statements whether you think it can always be justified, never be justified, or something in between, using this card. Sex before marriage</p>
<p><strong>Processed data type:</strong> integer/numeric</p>
<p><strong>Processed values:</strong></p>
<ul>
<li>1 = Never justifiable</li>
<li>2 = 2</li>
<li>3 = 3</li>
<li>4 = 4</li>
<li>5 = 5</li>
<li>6 = 6</li>
<li>7 = 7</li>
<li>8 = 8</li>
<li>9 = 9</li>
<li>10 = Always justifiable</li>
</ul>
<p><strong>Processing note:</strong> Original numeric values retained; all values below 0 are set to NA.</p>
</div>

<div class="codebook-variable">
<h3>Q187: Justification for suicide</h3>
<p><strong>Question:</strong> Please tell me for each of the following statements whether you think it can always be justified, never be justified, or something in between, using this card. Suicide</p>
<p><strong>Processed data type:</strong> integer/numeric</p>
<p><strong>Processed values:</strong></p>
<ul>
<li>1 = Never justifiable</li>
<li>2 = 2</li>
<li>3 = 3</li>
<li>4 = 4</li>
<li>5 = 5</li>
<li>6 = 6</li>
<li>7 = 7</li>
<li>8 = 8</li>
<li>9 = 9</li>
<li>10 = Always justifiable</li>
</ul>
<p><strong>Processing note:</strong> Original numeric values retained; all values below 0 are set to NA.</p>
</div>

<div class="codebook-variable">
<h3>Q188: Justification for euthanasia</h3>
<p><strong>Question:</strong> Please tell me for each of the following statements whether you think it can always be justified, never be justified, or something in between, using this card. Euthanasia</p>
<p><strong>Processed data type:</strong> integer/numeric</p>
<p><strong>Processed values:</strong></p>
<ul>
<li>1 = Never justifiable</li>
<li>2 = 2</li>
<li>3 = 3</li>
<li>4 = 4</li>
<li>5 = 5</li>
<li>6 = 6</li>
<li>7 = 7</li>
<li>8 = 8</li>
<li>9 = 9</li>
<li>10 = Always justifiable</li>
</ul>
<p><strong>Processing note:</strong> Original numeric values retained; all values below 0 are set to NA.</p>
</div>

<div class="codebook-variable">
<h3>Q189: Justification for a man to beat his wife</h3>
<p><strong>Question:</strong> Please tell me for each of the following statements whether you think it can always be justified, never be justified, or something in between, using this card. For a man to beat his wife</p>
<p><strong>Processed data type:</strong> integer/numeric</p>
<p><strong>Processed values:</strong></p>
<ul>
<li>1 = Never justifiable</li>
<li>2 = 2</li>
<li>3 = 3</li>
<li>4 = 4</li>
<li>5 = 5</li>
<li>6 = 6</li>
<li>7 = 7</li>
<li>8 = 8</li>
<li>9 = 9</li>
<li>10 = Always justifiable</li>
</ul>
<p><strong>Processing note:</strong> Original numeric values retained; all values below 0 are set to NA.</p>
</div>

<div class="codebook-variable">
<h3>Q190: Justification for parents beating children</h3>
<p><strong>Question:</strong> Please tell me for each of the following statements whether you think it can always be justified, never be justified, or something in between, using this card. Parents beating children</p>
<p><strong>Processed data type:</strong> integer/numeric</p>
<p><strong>Processed values:</strong></p>
<ul>
<li>1 = Never justifiable</li>
<li>2 = 2</li>
<li>3 = 3</li>
<li>4 = 4</li>
<li>5 = 5</li>
<li>6 = 6</li>
<li>7 = 7</li>
<li>8 = 8</li>
<li>9 = 9</li>
<li>10 = Always justifiable</li>
</ul>
<p><strong>Processing note:</strong> Original numeric values retained; all values below 0 are set to NA.</p>
</div>

<div class="codebook-variable">
<h3>Q191: Justification for violence against other people</h3>
<p><strong>Question:</strong> Please tell me for each of the following statements whether you think it can always be justified, never be justified, or something in between, using this card. Violence against other people</p>
<p><strong>Processed data type:</strong> integer/numeric</p>
<p><strong>Processed values:</strong></p>
<ul>
<li>1 = Never justifiable</li>
<li>2 = 2</li>
<li>3 = 3</li>
<li>4 = 4</li>
<li>5 = 5</li>
<li>6 = 6</li>
<li>7 = 7</li>
<li>8 = 8</li>
<li>9 = 9</li>
<li>10 = Always justifiable</li>
</ul>
<p><strong>Processing note:</strong> Original numeric values retained; all values below 0 are set to NA.</p>
</div>

<div class="codebook-variable">
<h3>Q192: Justification for terrorism</h3>
<p><strong>Question:</strong> Please tell me for each of the following statements whether you think it can always be justified, never be justified, or something in between, using this card. Terrorism as a political, ideological or religious mean</p>
<p><strong>Processed data type:</strong> integer/numeric</p>
<p><strong>Processed values:</strong></p>
<ul>
<li>1 = Never justifiable</li>
<li>2 = 2</li>
<li>3 = 3</li>
<li>4 = 4</li>
<li>5 = 5</li>
<li>6 = 6</li>
<li>7 = 7</li>
<li>8 = 8</li>
<li>9 = 9</li>
<li>10 = Always justifiable</li>
</ul>
<p><strong>Processing note:</strong> Original numeric values retained; all values below 0 are set to NA.</p>
</div>

<div class="codebook-variable">
<h3>Q193: Justification for casual sex</h3>
<p><strong>Question:</strong> Please tell me for each of the following statements whether you think it can always be justified, never be justified, or something in between, using this card. Having casual sex</p>
<p><strong>Processed data type:</strong> integer/numeric</p>
<p><strong>Processed values:</strong></p>
<ul>
<li>1 = Never justifiable</li>
<li>2 = 2</li>
<li>3 = 3</li>
<li>4 = 4</li>
<li>5 = 5</li>
<li>6 = 6</li>
<li>7 = 7</li>
<li>8 = 8</li>
<li>9 = 9</li>
<li>10 = Always justifiable</li>
</ul>
<p><strong>Processing note:</strong> Original numeric values retained; all values below 0 are set to NA.</p>
</div>

<div class="codebook-variable">
<h3>Q194: Justification for political violence</h3>
<p><strong>Question:</strong> Please tell me for each of the following statements whether you think it can always be justified, never be justified, or something in between, using this card. Political violence</p>
<p><strong>Processed data type:</strong> integer/numeric</p>
<p><strong>Processed values:</strong></p>
<ul>
<li>1 = Never justifiable</li>
<li>2 = 2</li>
<li>3 = 3</li>
<li>4 = 4</li>
<li>5 = 5</li>
<li>6 = 6</li>
<li>7 = 7</li>
<li>8 = 8</li>
<li>9 = 9</li>
<li>10 = Always justifiable</li>
</ul>
<p><strong>Processing note:</strong> Original numeric values retained; all values below 0 are set to NA.</p>
</div>

<div class="codebook-variable">
<h3>Q195: Justification for the death penalty</h3>
<p><strong>Question:</strong> Please tell me for each of the following statements whether you think it can always be justified, never be justified, or something in between, using this card. Death penalty</p>
<p><strong>Processed data type:</strong> integer/numeric</p>
<p><strong>Processed values:</strong></p>
<ul>
<li>1 = Never justifiable</li>
<li>2 = 2</li>
<li>3 = 3</li>
<li>4 = 4</li>
<li>5 = 5</li>
<li>6 = 6</li>
<li>7 = 7</li>
<li>8 = 8</li>
<li>9 = 9</li>
<li>10 = Always justifiable</li>
</ul>
<p><strong>Processing note:</strong> Original numeric values retained; all values below 0 are set to NA.</p>
</div>

</details>

<details class="codebook-section">
<summary><strong>DEMOGRAPHICS</strong> <span class="text-muted">(6 variables)</span></summary>

<div class="codebook-variable">
<h3>Q260: Respondent&#x27;s sex</h3>
<p><strong>Question:</strong> Respondent&#x27;s sex</p>
<p><strong>Processed data type:</strong> numeric</p>
<p><strong>Processed values:</strong></p>
<ul>
<li>0 = Female</li>
<li>1 = Male</li>
</ul>
<p><strong>Processing note:</strong> Custom recode in setup script; all unmatched responses and original negative missing codes are set to NA.</p>
</div>

<div class="codebook-variable">
<h3>Q262: Age</h3>
<p><strong>Question:</strong> This means you are XX years old? Numeric variable - numbers of years</p>
<p><strong>Processed data type:</strong> integer/numeric</p>
<p><strong>Processed values:</strong></p>
<ul>
<li>1 = 16-24 years</li>
<li>2 = 25-34 years</li>
<li>3 = 35-44 years</li>
<li>4 = 45-54 years</li>
<li>5 = 55-64 years</li>
<li>6 = 65 and over</li>
<li>1 = 16-29 years</li>
<li>2 = 30-49 years</li>
<li>3 = 50 years and over</li>
</ul>
<p><strong>Processing note:</strong> Original numeric values retained; all values below 0 are set to NA.</p>
</div>

<div class="codebook-variable">
<h3>Q263: Born in this country or immigrant</h3>
<p><strong>Question:</strong> Were you born in this country or are you an immigrant?</p>
<p><strong>Processed data type:</strong> numeric</p>
<p><strong>Processed values:</strong></p>
<ul>
<li>0 = I am an immigrant to this country (born outside this country)</li>
<li>1 = I am born in this country</li>
</ul>
<p><strong>Processing note:</strong> Custom recode in setup script; all unmatched responses and original negative missing codes are set to NA.</p>
</div>

<div class="codebook-variable">
<h3>Q275: Highest educational level of respondent</h3>
<p><strong>Question:</strong> What is the highest educational level that you have attained?</p>
<p><strong>Processed data type:</strong> ordered numeric in indiv_ordinal; ordered factor in intermediate indiv</p>
<p><strong>Processed values:</strong></p>
<ul>
<li>1 = Early childhood education (ISCED 0) / no education (original 0)</li>
<li>2 = Primary education (ISCED 1) (original 1)</li>
<li>3 = Lower (original 1)</li>
<li>4 = Lower secondary education (ISCED 2) (original 2)</li>
<li>5 = Middle (original 2)</li>
<li>6 = Upper secondary education (ISCED 3) (original 3)</li>
<li>7 = Higher (original 3)</li>
<li>8 = Post-secondary non-tertiary education (ISCED 4) (original 4)</li>
<li>9 = Short-cycle tertiary education (ISCED 5) (original 5)</li>
<li>10 = Bachelor or equivalent (ISCED 6) (original 6)</li>
<li>11 = Master or equivalent (ISCED 7) (original 7)</li>
<li>12 = Doctoral or equivalent (ISCED 8) (original 8)</li>
</ul>
<p><strong>Processing note:</strong> Haven labelled values converted to an ordered factor, then ordered factors are converted to ordinal integers with negative missing codes set to NA.</p>
</div>

<div class="codebook-variable">
<h3>Q288: Income scale placement</h3>
<p><strong>Question:</strong> On this card is an income scale on which 1 indicates the lowest income group and 10 the highest income group in your country. We would like to know in what group your household is. Please, specify the appropriate number, counting all wages, salaries, pensions and other incomes that come in.</p>
<p><strong>Processed data type:</strong> integer/numeric</p>
<p><strong>Processed values:</strong></p>
<ul>
<li>1 = Lower step</li>
<li>2 = second step</li>
<li>3 = Third step</li>
<li>4 = Fourth step</li>
<li>5 = Fifth step</li>
<li>6 = Sixth step</li>
<li>7 = Seventh step</li>
<li>8 = Eight step</li>
<li>9 = Nineth step</li>
<li>10 = Tenth step</li>
<li>1 = Low</li>
<li>2 = Medium</li>
<li>3 = High</li>
</ul>
<p><strong>Processing note:</strong> Original numeric values retained; all values below 0 are set to NA.</p>
</div>

<div class="codebook-variable">
<h3>Q289: Belonging to a religion or religious denomination</h3>
<p><strong>Question:</strong> Do you belong to a religion or religious denomination? If yes, which one?</p>
<p><strong>Processed data type:</strong> numeric</p>
<p><strong>Processed values:</strong></p>
<ul>
<li>0 = Other</li>
<li>1 = Other Christian (Jehova withness...)</li>
<li>2 = Buddhist</li>
<li>3 = Hindu</li>
<li>4 = Muslim</li>
<li>5 = Jew</li>
<li>6 = Orthodox (Russian/Greek/etc.)</li>
<li>7 = Protestant</li>
<li>8 = Catholic (Roman/Greek/etc)</li>
<li>9 = Do not belong to a denomination</li>
</ul>
<p><strong>Processing note:</strong> Custom recode in setup script; all unmatched responses and original negative missing codes are set to NA.</p>
</div>

</details>
