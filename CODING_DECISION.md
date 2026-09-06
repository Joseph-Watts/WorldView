# WorldView coding decision

WorldView uses one user-facing coding rule:

**For binary and ordinal variables, the processed numeric value is the
authoritative value and determines the displayed numerical order.**

There is no separate user-facing correlation coding scheme.

| Variable | Processed coding |
|---|---|
| Q165 Believe in God | 0 = No; 1 = Yes |
| Q166 Believe in life after death | 0 = No; 1 = Yes |
| Q167 Believe in hell | 0 = No; 1 = Yes |
| Q168 Believe in heaven | 0 = No; 1 = Yes |
| Q260 Sex | 0 = Female; 1 = Male |
| Q263 Respondent immigrant status | 0 = Immigrant; 1 = Born in country |

Q263 uses a neutral "status" label rather than reversing its 0/1 coding. This
keeps the existing WorldView numerical dataset and model outputs unchanged.
