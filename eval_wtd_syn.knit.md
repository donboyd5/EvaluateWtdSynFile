---
title: "Evaluation of weighted synthetic file"
output:
  html_document:
    toc: true
    toc_depth: 4
    toc_float: true
editor_options: 
  chunk_output_type: console
---









































# Initial simple checks
## Sum of weights and wages

```r
stack %>% group_by(ftype) %>% summarise(n = n(), wt = sum(wt), e00200 = sum(e00200)) %>% kable(caption = "Unweighted sums", 
    digits = c(0), format.args = list(big.mark = ","))
```



Table: Unweighted sums

ftype             n            wt           e00200
---------  --------  ------------  ---------------
train        16,379    14,504,831    3,710,957,450
test         16,379    14,534,325    3,559,263,412
synpuf1      16,379     3,669,124    5,602,434,987
synpuf3      16,379    14,467,624    3,469,232,079
puf.full    163,786   145,161,989   35,956,200,174
synpuf2     163,786    32,192,578   54,647,985,112
synpuf4     163,786   145,548,903   32,030,102,623


## Quantiles for weight

Table: Unweighted quantiles of wt

ftype             n   n.notNA    0%    10%    25%       50%       75%       90%      100%
---------  --------  --------  ----  -----  -----  --------  --------  --------  --------
train        16,379    16,379   7.1   10.1   40.3   1,425.9   1,437.0   1,451.7   2,014.1
test         16,379    16,379   7.1   10.1   40.8   1,425.9   1,437.0   1,451.7   2,014.1
synpuf1      16,379    16,379   9.3   10.0   10.2      41.0     145.4   1,395.5   1,859.6
synpuf3      16,379    16,379   7.1   10.1   40.8   1,425.9   1,437.0   1,451.7   2,014.1
puf.full    163,786   163,786   7.1   10.1   40.8   1,425.9   1,437.0   1,451.7   2,014.1
synpuf2     163,786   163,786   7.1   10.0   10.2      41.0     145.4     691.4   2,014.1
synpuf4     163,786   163,786   7.1   10.1   40.8   1,425.9   1,437.0   1,451.7   2,014.1





## Weighted sums of variables
* Values other than the pdiffs are in $ billions
* diff2 and diff4 are synpuf2 and synpuf4 minus puf.full
* pdiff2 and pdiff4 are diffs as % of puf.full

Table: Weighted aggregates in $ billions, sorted by abs of: puf.full

variable    puf.full   synpuf2   synpuf4    diff2    diff4   pdiff2   pdiff4  vdesc                                                         
---------  ---------  --------  --------  -------  -------  -------  -------  --------------------------------------------------------------
e00200        6044.8    6019.3    5359.9    -25.5   -684.9     -0.4    -11.3  Salaries and wages                                            
e01500         905.2     896.0    1248.7     -9.2    343.5     -1.0     38.0  Total pensions and annuities received                         
e01700         579.2     311.0     937.2   -268.2    358.0    -46.3     61.8  Pensions and annuities included in AGI                        
e02400         495.3     158.4     982.9   -336.9    487.6    -68.0     98.4  Gross Social Security benefits                                
e02000         466.9    1234.6     660.8    767.6    193.9    164.4     41.5  Schedule E net income or loss (+/-)                           
e26270         407.3     945.9     525.1    538.6    117.8    132.2     28.9  Combined partnership and S corporation net income/loss (+/-)  
e19200         380.2     372.1     478.6     -8.1     98.4     -2.1     25.9  Total interest paid deduction                                 
p23250         356.9     949.2     487.3    592.3    130.4    166.0     36.5  Long-term gains less losses                                   
e00900         279.6     281.2     268.9      1.7    -10.7      0.6     -3.8  Business or profession (Schedule C) net profit/loss (+/-)     
e18400         274.9     579.0     323.9    304.1     49.1    110.6     17.8  State and local taxes                                         
e01400         219.1     462.7     452.0    243.6    233.0    111.2    106.3  Taxable IRA distribution                                      
e00600         182.3     553.1     342.0    370.8    159.7    203.4     87.6  Dividends included in AGI                                     
e18500         173.1     234.5     281.8     61.4    108.6     35.4     62.8  Real estate tax deductions                                    
e19800         134.9     225.1     215.5     90.2     80.5     66.8     59.7  Cash contributions                                            
e17500         130.7      97.3     219.5    -33.4     88.8    -25.6     67.9  Medical and dental expenses subject to reduction by AGI limit 
e00650         130.6     367.8     232.8    237.2    102.3    181.7     78.3  Qualified Dividends                                           
e20400         120.7     309.4     132.9    188.7     12.2    156.4     10.1  Miscellaneous deductions subject to AGI limitation, total     
e00300         113.2     260.1     113.8    146.9      0.6    129.8      0.5  Interest received                                             
e02300          92.6      17.7      98.8    -74.9      6.3    -80.9      6.8  Unemployment compensation in AGI                              
e00400          70.3     328.1     170.8    257.8    100.5    366.9    143.0  Tax-exempt interest income                                    
e20100          34.6      86.9      57.1     52.3     22.5    151.2     65.2  Other than cash contributions                                 
e87521          29.2       1.2      34.1    -28.0      4.9    -95.9     16.7  American Opportunity Credit                                   
e00700          26.9      43.8      35.7     16.9      8.8     62.9     32.7  State income tax refunds                                      
e03270          24.7      33.1      39.1      8.4     14.4     34.2     58.5  Self-employed health insurance deduction                      
e03300          19.0      68.3      23.7     49.2      4.7    258.5     24.7  Payments to KEOGH accounts                                    
e32800          18.1       1.2      22.7    -17.0      4.6    -93.5     25.3  Qualifying individuals' Expenses                              
e01200         -17.5     -17.7     -13.8     -0.2      3.7      1.0    -21.2  Other gains (or loss)  (+/-)                                  
e87530          16.2       1.7      44.3    -14.6     28.1    -89.8    173.1  Lifetime Learning Total Qualified Expenses                    
p22250         -15.6     -74.7     -37.7    -59.1    -22.1    380.1    142.1  Short-term gains less losses                                  
e07300          14.0      26.8       9.8     12.8     -4.2     91.1    -29.8  Foreign tax                                                   
e62900          12.6      24.6       8.9     12.0     -3.8     94.6    -29.8  Alternative tax foreign tax credit                            
e24515          11.3      47.5      16.2     36.2      4.9    319.5     43.5  Unrecaptured Section 1250 gain                                
e03150          11.1       8.9      21.2     -2.2     10.0    -20.2     90.3  Total deductible individual retirement account (IRA) payments 
e03210           9.7       0.2       8.0     -9.5     -1.7    -97.5    -17.7  Student Loan Interest Deduction                               
e02100          -9.3     -20.4     -21.2    -11.1    -11.9    119.6    127.8  Schedule F net profit/loss (+/-)                              
e03240           8.1      21.6      13.9     13.4      5.8    165.0     71.2  Domestic Production Activities deduction                      
e00800           7.6       2.4      10.5     -5.3      2.9    -68.9     37.8  Alimony received                                              
e03500           7.2      22.4       8.5     15.3      1.3    213.4     18.8  Alimony paid                                                  
e09900           5.8       3.6       2.8     -2.2     -3.0    -37.7    -51.6  Penalty tax on IRA                                            
e27200           4.7      17.1      10.9     12.5      6.2    266.4    133.2  Farm rent net income or loss (+/-)                            
e03230           4.2       0.3       7.8     -3.9      3.6    -93.2     86.3  Tuition and Fees Deduction                                    
e24518           3.9       8.3       4.1      4.4      0.2    112.2      4.3  28% Rate Gain or Loss                                         
e03290           3.1       3.9       5.5      0.7      2.4     23.2     76.6  Health Savings Account deduction                              
e07400           2.4      10.4       3.5      8.0      1.1    330.8     44.8  General business credit                                       
e58990           2.2      22.8       2.3     20.6      0.1    940.8      5.9  Investment income (Form 4952 part 2 line 4g)                  
e01100           1.7       0.7       2.5     -1.0      0.8    -58.6     43.6  Capital gain distributions reported on Form 1040              
e11200           1.6       5.9       1.5      4.3      0.0    271.1     -2.5  Excess FICA/RRTA                                              
e07260           1.5       2.3       2.2      0.7      0.6     47.7     41.4  Residential Energy Credit                                     
e07240           1.1       0.1       1.6     -1.0      0.5    -93.2     43.3  Retirement Savings Credit                                     
e03220           1.0       0.1       1.9     -0.9      0.9    -91.2     92.0  Educator Expenses                                             
e07600           0.5       3.5       0.8      3.0      0.3    597.8     65.5  Credit for prior year minimum tax                             
e03400           0.5       0.4       0.5     -0.1      0.0    -12.4      5.4  Forfeited interest penalty                                    
p08000           0.1       0.3       0.3      0.1      0.2    117.2    130.5  Other Credits                                                 
e09700           0.0       0.2       0.0      0.2      0.0   1278.8     28.0  Recapture taxes                                               
e09800           0.0       0.0       0.0      0.0      0.0    -91.3    127.2  Social security tax on tip income                             



Table: Weighted aggregates in $ billions, sorted by abs of: diff2

variable    puf.full   synpuf2   synpuf4    diff2    diff4   pdiff2   pdiff4  vdesc                                                         
---------  ---------  --------  --------  -------  -------  -------  -------  --------------------------------------------------------------
e02000         466.9    1234.6     660.8    767.6    193.9    164.4     41.5  Schedule E net income or loss (+/-)                           
p23250         356.9     949.2     487.3    592.3    130.4    166.0     36.5  Long-term gains less losses                                   
e26270         407.3     945.9     525.1    538.6    117.8    132.2     28.9  Combined partnership and S corporation net income/loss (+/-)  
e00600         182.3     553.1     342.0    370.8    159.7    203.4     87.6  Dividends included in AGI                                     
e02400         495.3     158.4     982.9   -336.9    487.6    -68.0     98.4  Gross Social Security benefits                                
e18400         274.9     579.0     323.9    304.1     49.1    110.6     17.8  State and local taxes                                         
e01700         579.2     311.0     937.2   -268.2    358.0    -46.3     61.8  Pensions and annuities included in AGI                        
e00400          70.3     328.1     170.8    257.8    100.5    366.9    143.0  Tax-exempt interest income                                    
e01400         219.1     462.7     452.0    243.6    233.0    111.2    106.3  Taxable IRA distribution                                      
e00650         130.6     367.8     232.8    237.2    102.3    181.7     78.3  Qualified Dividends                                           
e20400         120.7     309.4     132.9    188.7     12.2    156.4     10.1  Miscellaneous deductions subject to AGI limitation, total     
e00300         113.2     260.1     113.8    146.9      0.6    129.8      0.5  Interest received                                             
e19800         134.9     225.1     215.5     90.2     80.5     66.8     59.7  Cash contributions                                            
e02300          92.6      17.7      98.8    -74.9      6.3    -80.9      6.8  Unemployment compensation in AGI                              
e18500         173.1     234.5     281.8     61.4    108.6     35.4     62.8  Real estate tax deductions                                    
p22250         -15.6     -74.7     -37.7    -59.1    -22.1    380.1    142.1  Short-term gains less losses                                  
e20100          34.6      86.9      57.1     52.3     22.5    151.2     65.2  Other than cash contributions                                 
e03300          19.0      68.3      23.7     49.2      4.7    258.5     24.7  Payments to KEOGH accounts                                    
e24515          11.3      47.5      16.2     36.2      4.9    319.5     43.5  Unrecaptured Section 1250 gain                                
e17500         130.7      97.3     219.5    -33.4     88.8    -25.6     67.9  Medical and dental expenses subject to reduction by AGI limit 
e87521          29.2       1.2      34.1    -28.0      4.9    -95.9     16.7  American Opportunity Credit                                   
e00200        6044.8    6019.3    5359.9    -25.5   -684.9     -0.4    -11.3  Salaries and wages                                            
e58990           2.2      22.8       2.3     20.6      0.1    940.8      5.9  Investment income (Form 4952 part 2 line 4g)                  
e32800          18.1       1.2      22.7    -17.0      4.6    -93.5     25.3  Qualifying individuals' Expenses                              
e00700          26.9      43.8      35.7     16.9      8.8     62.9     32.7  State income tax refunds                                      
e03500           7.2      22.4       8.5     15.3      1.3    213.4     18.8  Alimony paid                                                  
e87530          16.2       1.7      44.3    -14.6     28.1    -89.8    173.1  Lifetime Learning Total Qualified Expenses                    
e03240           8.1      21.6      13.9     13.4      5.8    165.0     71.2  Domestic Production Activities deduction                      
e07300          14.0      26.8       9.8     12.8     -4.2     91.1    -29.8  Foreign tax                                                   
e27200           4.7      17.1      10.9     12.5      6.2    266.4    133.2  Farm rent net income or loss (+/-)                            
e62900          12.6      24.6       8.9     12.0     -3.8     94.6    -29.8  Alternative tax foreign tax credit                            
e02100          -9.3     -20.4     -21.2    -11.1    -11.9    119.6    127.8  Schedule F net profit/loss (+/-)                              
e03210           9.7       0.2       8.0     -9.5     -1.7    -97.5    -17.7  Student Loan Interest Deduction                               
e01500         905.2     896.0    1248.7     -9.2    343.5     -1.0     38.0  Total pensions and annuities received                         
e03270          24.7      33.1      39.1      8.4     14.4     34.2     58.5  Self-employed health insurance deduction                      
e19200         380.2     372.1     478.6     -8.1     98.4     -2.1     25.9  Total interest paid deduction                                 
e07400           2.4      10.4       3.5      8.0      1.1    330.8     44.8  General business credit                                       
e00800           7.6       2.4      10.5     -5.3      2.9    -68.9     37.8  Alimony received                                              
e24518           3.9       8.3       4.1      4.4      0.2    112.2      4.3  28% Rate Gain or Loss                                         
e11200           1.6       5.9       1.5      4.3      0.0    271.1     -2.5  Excess FICA/RRTA                                              
e03230           4.2       0.3       7.8     -3.9      3.6    -93.2     86.3  Tuition and Fees Deduction                                    
e07600           0.5       3.5       0.8      3.0      0.3    597.8     65.5  Credit for prior year minimum tax                             
e03150          11.1       8.9      21.2     -2.2     10.0    -20.2     90.3  Total deductible individual retirement account (IRA) payments 
e09900           5.8       3.6       2.8     -2.2     -3.0    -37.7    -51.6  Penalty tax on IRA                                            
e00900         279.6     281.2     268.9      1.7    -10.7      0.6     -3.8  Business or profession (Schedule C) net profit/loss (+/-)     
e07240           1.1       0.1       1.6     -1.0      0.5    -93.2     43.3  Retirement Savings Credit                                     
e01100           1.7       0.7       2.5     -1.0      0.8    -58.6     43.6  Capital gain distributions reported on Form 1040              
e03220           1.0       0.1       1.9     -0.9      0.9    -91.2     92.0  Educator Expenses                                             
e07260           1.5       2.3       2.2      0.7      0.6     47.7     41.4  Residential Energy Credit                                     
e03290           3.1       3.9       5.5      0.7      2.4     23.2     76.6  Health Savings Account deduction                              
e09700           0.0       0.2       0.0      0.2      0.0   1278.8     28.0  Recapture taxes                                               
e01200         -17.5     -17.7     -13.8     -0.2      3.7      1.0    -21.2  Other gains (or loss)  (+/-)                                  
p08000           0.1       0.3       0.3      0.1      0.2    117.2    130.5  Other Credits                                                 
e03400           0.5       0.4       0.5     -0.1      0.0    -12.4      5.4  Forfeited interest penalty                                    
e09800           0.0       0.0       0.0      0.0      0.0    -91.3    127.2  Social security tax on tip income                             



Table: Weighted aggregates in $ billions, sorted by abs of: pdiff2

variable    puf.full   synpuf2   synpuf4    diff2    diff4   pdiff2   pdiff4  vdesc                                                         
---------  ---------  --------  --------  -------  -------  -------  -------  --------------------------------------------------------------
e09700           0.0       0.2       0.0      0.2      0.0   1278.8     28.0  Recapture taxes                                               
e58990           2.2      22.8       2.3     20.6      0.1    940.8      5.9  Investment income (Form 4952 part 2 line 4g)                  
e07600           0.5       3.5       0.8      3.0      0.3    597.8     65.5  Credit for prior year minimum tax                             
p22250         -15.6     -74.7     -37.7    -59.1    -22.1    380.1    142.1  Short-term gains less losses                                  
e00400          70.3     328.1     170.8    257.8    100.5    366.9    143.0  Tax-exempt interest income                                    
e07400           2.4      10.4       3.5      8.0      1.1    330.8     44.8  General business credit                                       
e24515          11.3      47.5      16.2     36.2      4.9    319.5     43.5  Unrecaptured Section 1250 gain                                
e11200           1.6       5.9       1.5      4.3      0.0    271.1     -2.5  Excess FICA/RRTA                                              
e27200           4.7      17.1      10.9     12.5      6.2    266.4    133.2  Farm rent net income or loss (+/-)                            
e03300          19.0      68.3      23.7     49.2      4.7    258.5     24.7  Payments to KEOGH accounts                                    
e03500           7.2      22.4       8.5     15.3      1.3    213.4     18.8  Alimony paid                                                  
e00600         182.3     553.1     342.0    370.8    159.7    203.4     87.6  Dividends included in AGI                                     
e00650         130.6     367.8     232.8    237.2    102.3    181.7     78.3  Qualified Dividends                                           
p23250         356.9     949.2     487.3    592.3    130.4    166.0     36.5  Long-term gains less losses                                   
e03240           8.1      21.6      13.9     13.4      5.8    165.0     71.2  Domestic Production Activities deduction                      
e02000         466.9    1234.6     660.8    767.6    193.9    164.4     41.5  Schedule E net income or loss (+/-)                           
e20400         120.7     309.4     132.9    188.7     12.2    156.4     10.1  Miscellaneous deductions subject to AGI limitation, total     
e20100          34.6      86.9      57.1     52.3     22.5    151.2     65.2  Other than cash contributions                                 
e26270         407.3     945.9     525.1    538.6    117.8    132.2     28.9  Combined partnership and S corporation net income/loss (+/-)  
e00300         113.2     260.1     113.8    146.9      0.6    129.8      0.5  Interest received                                             
e02100          -9.3     -20.4     -21.2    -11.1    -11.9    119.6    127.8  Schedule F net profit/loss (+/-)                              
p08000           0.1       0.3       0.3      0.1      0.2    117.2    130.5  Other Credits                                                 
e24518           3.9       8.3       4.1      4.4      0.2    112.2      4.3  28% Rate Gain or Loss                                         
e01400         219.1     462.7     452.0    243.6    233.0    111.2    106.3  Taxable IRA distribution                                      
e18400         274.9     579.0     323.9    304.1     49.1    110.6     17.8  State and local taxes                                         
e03210           9.7       0.2       8.0     -9.5     -1.7    -97.5    -17.7  Student Loan Interest Deduction                               
e87521          29.2       1.2      34.1    -28.0      4.9    -95.9     16.7  American Opportunity Credit                                   
e62900          12.6      24.6       8.9     12.0     -3.8     94.6    -29.8  Alternative tax foreign tax credit                            
e32800          18.1       1.2      22.7    -17.0      4.6    -93.5     25.3  Qualifying individuals' Expenses                              
e07240           1.1       0.1       1.6     -1.0      0.5    -93.2     43.3  Retirement Savings Credit                                     
e03230           4.2       0.3       7.8     -3.9      3.6    -93.2     86.3  Tuition and Fees Deduction                                    
e09800           0.0       0.0       0.0      0.0      0.0    -91.3    127.2  Social security tax on tip income                             
e03220           1.0       0.1       1.9     -0.9      0.9    -91.2     92.0  Educator Expenses                                             
e07300          14.0      26.8       9.8     12.8     -4.2     91.1    -29.8  Foreign tax                                                   
e87530          16.2       1.7      44.3    -14.6     28.1    -89.8    173.1  Lifetime Learning Total Qualified Expenses                    
e02300          92.6      17.7      98.8    -74.9      6.3    -80.9      6.8  Unemployment compensation in AGI                              
e00800           7.6       2.4      10.5     -5.3      2.9    -68.9     37.8  Alimony received                                              
e02400         495.3     158.4     982.9   -336.9    487.6    -68.0     98.4  Gross Social Security benefits                                
e19800         134.9     225.1     215.5     90.2     80.5     66.8     59.7  Cash contributions                                            
e00700          26.9      43.8      35.7     16.9      8.8     62.9     32.7  State income tax refunds                                      
e01100           1.7       0.7       2.5     -1.0      0.8    -58.6     43.6  Capital gain distributions reported on Form 1040              
e07260           1.5       2.3       2.2      0.7      0.6     47.7     41.4  Residential Energy Credit                                     
e01700         579.2     311.0     937.2   -268.2    358.0    -46.3     61.8  Pensions and annuities included in AGI                        
e09900           5.8       3.6       2.8     -2.2     -3.0    -37.7    -51.6  Penalty tax on IRA                                            
e18500         173.1     234.5     281.8     61.4    108.6     35.4     62.8  Real estate tax deductions                                    
e03270          24.7      33.1      39.1      8.4     14.4     34.2     58.5  Self-employed health insurance deduction                      
e17500         130.7      97.3     219.5    -33.4     88.8    -25.6     67.9  Medical and dental expenses subject to reduction by AGI limit 
e03290           3.1       3.9       5.5      0.7      2.4     23.2     76.6  Health Savings Account deduction                              
e03150          11.1       8.9      21.2     -2.2     10.0    -20.2     90.3  Total deductible individual retirement account (IRA) payments 
e03400           0.5       0.4       0.5     -0.1      0.0    -12.4      5.4  Forfeited interest penalty                                    
e19200         380.2     372.1     478.6     -8.1     98.4     -2.1     25.9  Total interest paid deduction                                 
e01500         905.2     896.0    1248.7     -9.2    343.5     -1.0     38.0  Total pensions and annuities received                         
e01200         -17.5     -17.7     -13.8     -0.2      3.7      1.0    -21.2  Other gains (or loss)  (+/-)                                  
e00900         279.6     281.2     268.9      1.7    -10.7      0.6     -3.8  Business or profession (Schedule C) net profit/loss (+/-)     
e00200        6044.8    6019.3    5359.9    -25.5   -684.9     -0.4    -11.3  Salaries and wages                                            



Table: Weighted aggregates in $ billions, sorted by abs of: diff4

variable    puf.full   synpuf2   synpuf4    diff2    diff4   pdiff2   pdiff4  vdesc                                                         
---------  ---------  --------  --------  -------  -------  -------  -------  --------------------------------------------------------------
e00200        6044.8    6019.3    5359.9    -25.5   -684.9     -0.4    -11.3  Salaries and wages                                            
e02400         495.3     158.4     982.9   -336.9    487.6    -68.0     98.4  Gross Social Security benefits                                
e01700         579.2     311.0     937.2   -268.2    358.0    -46.3     61.8  Pensions and annuities included in AGI                        
e01500         905.2     896.0    1248.7     -9.2    343.5     -1.0     38.0  Total pensions and annuities received                         
e01400         219.1     462.7     452.0    243.6    233.0    111.2    106.3  Taxable IRA distribution                                      
e02000         466.9    1234.6     660.8    767.6    193.9    164.4     41.5  Schedule E net income or loss (+/-)                           
e00600         182.3     553.1     342.0    370.8    159.7    203.4     87.6  Dividends included in AGI                                     
p23250         356.9     949.2     487.3    592.3    130.4    166.0     36.5  Long-term gains less losses                                   
e26270         407.3     945.9     525.1    538.6    117.8    132.2     28.9  Combined partnership and S corporation net income/loss (+/-)  
e18500         173.1     234.5     281.8     61.4    108.6     35.4     62.8  Real estate tax deductions                                    
e00650         130.6     367.8     232.8    237.2    102.3    181.7     78.3  Qualified Dividends                                           
e00400          70.3     328.1     170.8    257.8    100.5    366.9    143.0  Tax-exempt interest income                                    
e19200         380.2     372.1     478.6     -8.1     98.4     -2.1     25.9  Total interest paid deduction                                 
e17500         130.7      97.3     219.5    -33.4     88.8    -25.6     67.9  Medical and dental expenses subject to reduction by AGI limit 
e19800         134.9     225.1     215.5     90.2     80.5     66.8     59.7  Cash contributions                                            
e18400         274.9     579.0     323.9    304.1     49.1    110.6     17.8  State and local taxes                                         
e87530          16.2       1.7      44.3    -14.6     28.1    -89.8    173.1  Lifetime Learning Total Qualified Expenses                    
e20100          34.6      86.9      57.1     52.3     22.5    151.2     65.2  Other than cash contributions                                 
p22250         -15.6     -74.7     -37.7    -59.1    -22.1    380.1    142.1  Short-term gains less losses                                  
e03270          24.7      33.1      39.1      8.4     14.4     34.2     58.5  Self-employed health insurance deduction                      
e20400         120.7     309.4     132.9    188.7     12.2    156.4     10.1  Miscellaneous deductions subject to AGI limitation, total     
e02100          -9.3     -20.4     -21.2    -11.1    -11.9    119.6    127.8  Schedule F net profit/loss (+/-)                              
e00900         279.6     281.2     268.9      1.7    -10.7      0.6     -3.8  Business or profession (Schedule C) net profit/loss (+/-)     
e03150          11.1       8.9      21.2     -2.2     10.0    -20.2     90.3  Total deductible individual retirement account (IRA) payments 
e00700          26.9      43.8      35.7     16.9      8.8     62.9     32.7  State income tax refunds                                      
e02300          92.6      17.7      98.8    -74.9      6.3    -80.9      6.8  Unemployment compensation in AGI                              
e27200           4.7      17.1      10.9     12.5      6.2    266.4    133.2  Farm rent net income or loss (+/-)                            
e03240           8.1      21.6      13.9     13.4      5.8    165.0     71.2  Domestic Production Activities deduction                      
e24515          11.3      47.5      16.2     36.2      4.9    319.5     43.5  Unrecaptured Section 1250 gain                                
e87521          29.2       1.2      34.1    -28.0      4.9    -95.9     16.7  American Opportunity Credit                                   
e03300          19.0      68.3      23.7     49.2      4.7    258.5     24.7  Payments to KEOGH accounts                                    
e32800          18.1       1.2      22.7    -17.0      4.6    -93.5     25.3  Qualifying individuals' Expenses                              
e07300          14.0      26.8       9.8     12.8     -4.2     91.1    -29.8  Foreign tax                                                   
e62900          12.6      24.6       8.9     12.0     -3.8     94.6    -29.8  Alternative tax foreign tax credit                            
e01200         -17.5     -17.7     -13.8     -0.2      3.7      1.0    -21.2  Other gains (or loss)  (+/-)                                  
e03230           4.2       0.3       7.8     -3.9      3.6    -93.2     86.3  Tuition and Fees Deduction                                    
e09900           5.8       3.6       2.8     -2.2     -3.0    -37.7    -51.6  Penalty tax on IRA                                            
e00800           7.6       2.4      10.5     -5.3      2.9    -68.9     37.8  Alimony received                                              
e03290           3.1       3.9       5.5      0.7      2.4     23.2     76.6  Health Savings Account deduction                              
e03210           9.7       0.2       8.0     -9.5     -1.7    -97.5    -17.7  Student Loan Interest Deduction                               
e03500           7.2      22.4       8.5     15.3      1.3    213.4     18.8  Alimony paid                                                  
e07400           2.4      10.4       3.5      8.0      1.1    330.8     44.8  General business credit                                       
e03220           1.0       0.1       1.9     -0.9      0.9    -91.2     92.0  Educator Expenses                                             
e01100           1.7       0.7       2.5     -1.0      0.8    -58.6     43.6  Capital gain distributions reported on Form 1040              
e07260           1.5       2.3       2.2      0.7      0.6     47.7     41.4  Residential Energy Credit                                     
e00300         113.2     260.1     113.8    146.9      0.6    129.8      0.5  Interest received                                             
e07240           1.1       0.1       1.6     -1.0      0.5    -93.2     43.3  Retirement Savings Credit                                     
e07600           0.5       3.5       0.8      3.0      0.3    597.8     65.5  Credit for prior year minimum tax                             
e24518           3.9       8.3       4.1      4.4      0.2    112.2      4.3  28% Rate Gain or Loss                                         
p08000           0.1       0.3       0.3      0.1      0.2    117.2    130.5  Other Credits                                                 
e58990           2.2      22.8       2.3     20.6      0.1    940.8      5.9  Investment income (Form 4952 part 2 line 4g)                  
e11200           1.6       5.9       1.5      4.3      0.0    271.1     -2.5  Excess FICA/RRTA                                              
e03400           0.5       0.4       0.5     -0.1      0.0    -12.4      5.4  Forfeited interest penalty                                    
e09800           0.0       0.0       0.0      0.0      0.0    -91.3    127.2  Social security tax on tip income                             
e09700           0.0       0.2       0.0      0.2      0.0   1278.8     28.0  Recapture taxes                                               



Table: Weighted aggregates in $ billions, sorted by abs of: pdiff4

variable    puf.full   synpuf2   synpuf4    diff2    diff4   pdiff2   pdiff4  vdesc                                                         
---------  ---------  --------  --------  -------  -------  -------  -------  --------------------------------------------------------------
e87530          16.2       1.7      44.3    -14.6     28.1    -89.8    173.1  Lifetime Learning Total Qualified Expenses                    
e00400          70.3     328.1     170.8    257.8    100.5    366.9    143.0  Tax-exempt interest income                                    
p22250         -15.6     -74.7     -37.7    -59.1    -22.1    380.1    142.1  Short-term gains less losses                                  
e27200           4.7      17.1      10.9     12.5      6.2    266.4    133.2  Farm rent net income or loss (+/-)                            
p08000           0.1       0.3       0.3      0.1      0.2    117.2    130.5  Other Credits                                                 
e02100          -9.3     -20.4     -21.2    -11.1    -11.9    119.6    127.8  Schedule F net profit/loss (+/-)                              
e09800           0.0       0.0       0.0      0.0      0.0    -91.3    127.2  Social security tax on tip income                             
e01400         219.1     462.7     452.0    243.6    233.0    111.2    106.3  Taxable IRA distribution                                      
e02400         495.3     158.4     982.9   -336.9    487.6    -68.0     98.4  Gross Social Security benefits                                
e03220           1.0       0.1       1.9     -0.9      0.9    -91.2     92.0  Educator Expenses                                             
e03150          11.1       8.9      21.2     -2.2     10.0    -20.2     90.3  Total deductible individual retirement account (IRA) payments 
e00600         182.3     553.1     342.0    370.8    159.7    203.4     87.6  Dividends included in AGI                                     
e03230           4.2       0.3       7.8     -3.9      3.6    -93.2     86.3  Tuition and Fees Deduction                                    
e00650         130.6     367.8     232.8    237.2    102.3    181.7     78.3  Qualified Dividends                                           
e03290           3.1       3.9       5.5      0.7      2.4     23.2     76.6  Health Savings Account deduction                              
e03240           8.1      21.6      13.9     13.4      5.8    165.0     71.2  Domestic Production Activities deduction                      
e17500         130.7      97.3     219.5    -33.4     88.8    -25.6     67.9  Medical and dental expenses subject to reduction by AGI limit 
e07600           0.5       3.5       0.8      3.0      0.3    597.8     65.5  Credit for prior year minimum tax                             
e20100          34.6      86.9      57.1     52.3     22.5    151.2     65.2  Other than cash contributions                                 
e18500         173.1     234.5     281.8     61.4    108.6     35.4     62.8  Real estate tax deductions                                    
e01700         579.2     311.0     937.2   -268.2    358.0    -46.3     61.8  Pensions and annuities included in AGI                        
e19800         134.9     225.1     215.5     90.2     80.5     66.8     59.7  Cash contributions                                            
e03270          24.7      33.1      39.1      8.4     14.4     34.2     58.5  Self-employed health insurance deduction                      
e09900           5.8       3.6       2.8     -2.2     -3.0    -37.7    -51.6  Penalty tax on IRA                                            
e07400           2.4      10.4       3.5      8.0      1.1    330.8     44.8  General business credit                                       
e01100           1.7       0.7       2.5     -1.0      0.8    -58.6     43.6  Capital gain distributions reported on Form 1040              
e24515          11.3      47.5      16.2     36.2      4.9    319.5     43.5  Unrecaptured Section 1250 gain                                
e07240           1.1       0.1       1.6     -1.0      0.5    -93.2     43.3  Retirement Savings Credit                                     
e02000         466.9    1234.6     660.8    767.6    193.9    164.4     41.5  Schedule E net income or loss (+/-)                           
e07260           1.5       2.3       2.2      0.7      0.6     47.7     41.4  Residential Energy Credit                                     
e01500         905.2     896.0    1248.7     -9.2    343.5     -1.0     38.0  Total pensions and annuities received                         
e00800           7.6       2.4      10.5     -5.3      2.9    -68.9     37.8  Alimony received                                              
p23250         356.9     949.2     487.3    592.3    130.4    166.0     36.5  Long-term gains less losses                                   
e00700          26.9      43.8      35.7     16.9      8.8     62.9     32.7  State income tax refunds                                      
e07300          14.0      26.8       9.8     12.8     -4.2     91.1    -29.8  Foreign tax                                                   
e62900          12.6      24.6       8.9     12.0     -3.8     94.6    -29.8  Alternative tax foreign tax credit                            
e26270         407.3     945.9     525.1    538.6    117.8    132.2     28.9  Combined partnership and S corporation net income/loss (+/-)  
e09700           0.0       0.2       0.0      0.2      0.0   1278.8     28.0  Recapture taxes                                               
e19200         380.2     372.1     478.6     -8.1     98.4     -2.1     25.9  Total interest paid deduction                                 
e32800          18.1       1.2      22.7    -17.0      4.6    -93.5     25.3  Qualifying individuals' Expenses                              
e03300          19.0      68.3      23.7     49.2      4.7    258.5     24.7  Payments to KEOGH accounts                                    
e01200         -17.5     -17.7     -13.8     -0.2      3.7      1.0    -21.2  Other gains (or loss)  (+/-)                                  
e03500           7.2      22.4       8.5     15.3      1.3    213.4     18.8  Alimony paid                                                  
e18400         274.9     579.0     323.9    304.1     49.1    110.6     17.8  State and local taxes                                         
e03210           9.7       0.2       8.0     -9.5     -1.7    -97.5    -17.7  Student Loan Interest Deduction                               
e87521          29.2       1.2      34.1    -28.0      4.9    -95.9     16.7  American Opportunity Credit                                   
e00200        6044.8    6019.3    5359.9    -25.5   -684.9     -0.4    -11.3  Salaries and wages                                            
e20400         120.7     309.4     132.9    188.7     12.2    156.4     10.1  Miscellaneous deductions subject to AGI limitation, total     
e02300          92.6      17.7      98.8    -74.9      6.3    -80.9      6.8  Unemployment compensation in AGI                              
e58990           2.2      22.8       2.3     20.6      0.1    940.8      5.9  Investment income (Form 4952 part 2 line 4g)                  
e03400           0.5       0.4       0.5     -0.1      0.0    -12.4      5.4  Forfeited interest penalty                                    
e24518           3.9       8.3       4.1      4.4      0.2    112.2      4.3  28% Rate Gain or Loss                                         
e00900         279.6     281.2     268.9      1.7    -10.7      0.6     -3.8  Business or profession (Schedule C) net profit/loss (+/-)     
e11200           1.6       5.9       1.5      4.3      0.0    271.1     -2.5  Excess FICA/RRTA                                              
e00300         113.2     260.1     113.8    146.9      0.6    129.8      0.5  Interest received                                             


## How can synpuf2 weight be so far off and yet weighted wages are so close?


### Record count by wage group

Table: Wage-related values by wage group

wagegrp            n_puf.full   n_synpuf2   n_synpuf4     diff2    diff4   pdiff2   pdiff4
----------------  -----------  ----------  ----------  --------  -------  -------  -------
[0,2.5e+04)            74,645      36,276      75,812   -38,369    1,167    -51.4      1.6
[2.5e+04,5e+04)        24,720       6,786      27,188   -17,934    2,468    -72.5     10.0
[5e+04,7.5e+04)        13,664       5,952       9,916    -7,712   -3,748    -56.4    -27.4
[7.5e+04,1e+05)         8,526       5,121       7,389    -3,405   -1,137    -39.9    -13.3
[1e+05,2e+05)          14,291      23,411      14,727     9,120      436     63.8      3.1
[2e+05,5e+05)          12,650      46,868      13,997    34,218    1,347    270.5     10.6
[5e+05, Inf)           15,290      39,372      14,757    24,082     -533    157.5     -3.5
Total                 163,786     163,786     163,786         0        0      0.0      0.0


### Weighted records (i.e., returns) by wage group

Table: Wage-related values by wage group

wagegrp            wt.sum_puf.full   wt.sum_synpuf2   wt.sum_synpuf4          diff2          diff4   pdiff2   pdiff4
----------------  ----------------  ---------------  ---------------  -------------  -------------  -------  -------
[0,2.5e+04)           76,102,214.4        7,584,060     79,557,656.4    -68,518,154    3,455,442.0    -90.0      4.5
[2.5e+04,5e+04)       30,408,457.5        2,001,219     34,438,479.9    -28,407,239    4,030,022.3    -93.4     13.3
[5e+04,7.5e+04)       15,585,870.0        1,573,028     10,439,607.4    -14,012,842   -5,146,262.6    -89.9    -33.0
[7.5e+04,1e+05)        9,130,373.0        1,063,431      7,533,964.0     -8,066,942   -1,596,409.0    -88.4    -17.5
[1e+05,2e+05)         11,055,321.2        8,229,094     10,820,896.6     -2,826,228     -234,424.6    -25.6     -2.1
[2e+05,5e+05)          2,455,115.9        9,575,431      2,364,372.5      7,120,315      -90,743.4    290.0     -3.7
[5e+05, Inf)             424,636.7        2,166,315        393,926.2      1,741,678      -30,710.5    410.2     -7.2
Total                145,161,988.8       32,192,578    145,548,903.0   -112,969,411      386,914.2    -77.8      0.3


### Sum of weighted wages, in $ billions, by wage group

Table: Wage-related values by wage group

wagegrp            wagesb_puf.full   wagesb_synpuf2   wagesb_synpuf4      diff2    diff4   pdiff2   pdiff4
----------------  ----------------  ---------------  ---------------  ---------  -------  -------  -------
[0,2.5e+04)                  586.7             29.9            424.9     -556.8   -161.8    -94.9    -27.6
[2.5e+04,5e+04)            1,094.2             74.4          1,131.9   -1,019.7     37.8    -93.2      3.5
[5e+04,7.5e+04)              956.1             97.4            647.6     -858.7   -308.5    -89.8    -32.3
[7.5e+04,1e+05)              788.5             91.9            645.2     -696.6   -143.3    -88.3    -18.2
[1e+05,2e+05)              1,470.9          1,298.0          1,451.8     -172.9    -19.1    -11.8     -1.3
[2e+05,5e+05)                688.2          2,844.9            657.6    2,156.7    -30.6    313.4     -4.4
[5e+05, Inf)                 460.2          1,582.5            400.9    1,122.4    -59.3    243.9    -12.9
Total                      6,044.8          6,019.3          5,359.9      -25.5   -684.9     -0.4    -11.3


## Unweighted correlations

```r
# correlations names(stack)
cor1 <- stack %>% select(ftype, corvars) %>% group_by(ftype) %>% do(cordf(.[, -1])) %>% do(trimcor(.)) %>% separate(combo, 
    c("var1", "var2"), remove = FALSE)
# glimpse(cor1)

wtdpuf <- stack %>% filter(ftype == "puf.full") %>% select(corvars) %>% gather(variable, value, -wt) %>% group_by(variable) %>% 
    summarise(abwtd.sumb = sum(abs(value) * wt)/1e+09) %>% arrange(-abwtd.sumb)
# wtdpuf

cor.wtd <- cor1 %>% filter(ftype %in% c("puf.full", "synpuf2", "synpuf4")) %>% spread(ftype, value) %>% mutate(diff2 = synpuf2 - 
    puf.full, diff4 = synpuf4 - puf.full, awsumb1 = wtdpuf$abwtd.sumb[match(var1, wtdpuf$variable)], awsumb2 = wtdpuf$abwtd.sumb[match(var2, 
    wtdpuf$variable)]) %>% select(-var1, -var2)

cor.wtd %>% arrange(-abs(diff4)) %>% filter(row_number() <= 50) %>% kable(caption = "Correlations sorted by abs(diff4): Top 50", 
    digits = c(0, rep(3, 5), 1, 1), format.args = list(big.mark = ","))
```



Table: Correlations sorted by abs(diff4): Top 50

combo            puf.full   synpuf2   synpuf4    diff2    diff4   awsumb1   awsumb2
--------------  ---------  --------  --------  -------  -------  --------  --------
e00300-p23250       0.254     0.246     0.552   -0.009    0.297     113.2     466.1
e00300-e02000       0.040     0.042     0.289    0.002    0.249     113.2     764.0
e00300-e26270       0.027     0.021     0.265   -0.006    0.238     113.2     628.5
e00200-e00300       0.063     0.015     0.275   -0.048    0.212   6,044.8     113.2
e03230-e87521       0.010     0.052     0.206    0.042    0.196       4.2      29.2
e00300-e18500       0.257     0.248     0.443   -0.009    0.187     113.2     173.1
e01500-e01700       0.239     0.172     0.417   -0.066    0.178     905.2     579.2
e00300-e62900       0.101     0.078     0.278   -0.023    0.177     113.2      12.6
e00300-e20400       0.400     0.226     0.577   -0.174    0.177     113.2     120.7
e00300-e07300       0.091     0.070     0.261   -0.020    0.171     113.2      14.0
e00300-wt          -0.115    -0.084    -0.284    0.031   -0.168     113.2        NA
e00300-e18400       0.364     0.016     0.523   -0.348    0.158     113.2     274.9
e20400-p23250       0.310     0.142     0.467   -0.169    0.156     120.7     466.1
e00300-e03240       0.099     0.139     0.252    0.040    0.153     113.2       8.1
e19800-e62900       0.058     0.074     0.200    0.016    0.142     134.9      12.6
e00700-e19800       0.160     0.121     0.297   -0.038    0.137      26.9     134.9
e03240-e19800       0.108     0.134     0.244    0.026    0.137       8.1     134.9
e00700-e20100       0.053     0.088     0.188    0.035    0.136      26.9      34.6
e07300-e19800       0.053     0.071     0.187    0.018    0.134      14.0     134.9
e00700-e07400       0.159     0.136     0.289   -0.023    0.129      26.9       2.4
e18400-p23250       0.310     0.022     0.438   -0.288    0.128     274.9     466.1
e00300-e19800       0.241     0.212     0.366   -0.028    0.125     113.2     134.9
e00300-e00400       0.231     0.205     0.355   -0.026    0.124     113.2      70.3
e20100-p23250       0.136     0.096     0.259   -0.040    0.123      34.6     466.1
e00400-e19800       0.176     0.167     0.296   -0.008    0.121      70.3     134.9
e19200-e19800       0.115     0.110     0.233   -0.005    0.118     380.2     134.9
e07300-e20400       0.105     0.020     0.221   -0.085    0.116      14.0     120.7
e19200-p23250       0.197     0.143     0.310   -0.054    0.113     380.2     466.1
e00400-e07400       0.039     0.050     0.151    0.011    0.112      70.3       2.4
e00400-e07300       0.054     0.036     0.164   -0.017    0.111      70.3      14.0
e20100-e62900       0.030     0.035     0.140    0.005    0.110      34.6      12.6
e18400-e19800       0.231     0.036     0.341   -0.195    0.110     274.9     134.9
e00400-e01400       0.065     0.132     0.174    0.067    0.110      70.3     219.1
e18400-e20100       0.152     0.001     0.260   -0.151    0.108     274.9      34.6
e00400-e18400       0.217    -0.019     0.323   -0.237    0.106      70.3     274.9
e00400-e00700       0.136     0.099     0.241   -0.037    0.106      70.3      26.9
e03240-e62900       0.049     0.037     0.155   -0.012    0.106       8.1      12.6
e07300-e20100       0.027     0.032     0.131    0.005    0.105      14.0      34.6
e00300-e20100       0.170     0.125     0.274   -0.045    0.104     113.2      34.6
e03240-e07300       0.045     0.038     0.149   -0.008    0.104       8.1      14.0
e07300-e19200       0.063     0.056     0.166   -0.007    0.103      14.0     380.2
e03210-wt           0.109     0.053     0.210   -0.056    0.101       9.7        NA
e00400-e03240       0.087     0.068     0.188   -0.020    0.101      70.3       8.1
e07300-e07600       0.013     0.005     0.114   -0.008    0.101      14.0       0.5
e00300-e24515       0.052     0.141     0.153    0.089    0.101     113.2      11.3
e19200-e62900       0.071     0.057     0.172   -0.014    0.101     380.2      12.6
e00200-e20400       0.119    -0.028     0.218   -0.147    0.099   6,044.8     120.7
e01400-e07400       0.004     0.021     0.102    0.016    0.098     219.1       2.4
e18500-e20100       0.112     0.096     0.210   -0.016    0.098     173.1      34.6
e00700-e19200       0.124     0.087     0.222   -0.038    0.098      26.9     380.2

```r
cor.wtd %>% arrange(-abs(awsumb2)) %>% filter(row_number() <= 50) %>% kable(caption = "Correlations sorted by sum(abs(leftmost variable)): Top 50", 
    digits = c(0, rep(3, 5), 1, 1), format.args = list(big.mark = ","))
```



Table: Correlations sorted by sum(abs(leftmost variable)): Top 50

combo            puf.full   synpuf2   synpuf4    diff2    diff4   awsumb1   awsumb2
--------------  ---------  --------  --------  -------  -------  --------  --------
e00200-e01500       0.024     0.003     0.075   -0.020    0.051   6,044.8     905.2
e00300-e01500       0.005     0.022     0.059    0.017    0.055     113.2     905.2
e00400-e01500       0.018     0.044     0.090    0.026    0.072      70.3     905.2
e00600-e01500       0.006     0.022     0.055    0.016    0.049     182.3     905.2
e00650-e01500       0.004     0.017     0.049    0.013    0.045     130.6     905.2
e00700-e01500       0.004     0.015     0.033    0.010    0.029      26.9     905.2
e00800-e01500      -0.003     0.000    -0.002    0.003    0.002       7.6     905.2
e00900-e01500       0.000     0.025     0.018    0.025    0.017     385.9     905.2
e01100-e01500       0.002     0.001     0.002   -0.001    0.001       1.7     905.2
e01200-e01500       0.002     0.002     0.000    0.001   -0.001      43.2     905.2
e01400-e01500       0.045     0.048     0.096    0.003    0.050     219.1     905.2
e00200-e02000       0.052    -0.046     0.075   -0.098    0.023   6,044.8     764.0
e00300-e02000       0.040     0.042     0.289    0.002    0.249     113.2     764.0
e00400-e02000       0.085     0.063     0.166   -0.021    0.081      70.3     764.0
e00600-e02000       0.042     0.062     0.091    0.020    0.049     182.3     764.0
e00650-e02000       0.034     0.054     0.081    0.019    0.046     130.6     764.0
e00700-e02000       0.281     0.129     0.334   -0.152    0.053      26.9     764.0
e00800-e02000      -0.003    -0.003    -0.005    0.000   -0.002       7.6     764.0
e00900-e02000      -0.013    -0.028    -0.003   -0.016    0.010     385.9     764.0
e01100-e02000       0.011     0.029     0.023    0.018    0.012       1.7     764.0
e01200-e02000      -0.023    -0.087    -0.018   -0.064    0.005      43.2     764.0
e01400-e02000       0.009     0.025     0.075    0.016    0.066     219.1     764.0
e01500-e02000      -0.004    -0.013    -0.003   -0.009    0.001     905.2     764.0
e01700-e02000      -0.008     0.000    -0.004    0.008    0.003     579.2     764.0
e00200-e26270       0.051    -0.048     0.068   -0.099    0.017   6,044.8     628.5
e00300-e26270       0.027     0.021     0.265   -0.006    0.238     113.2     628.5
e00400-e26270       0.072     0.036     0.144   -0.036    0.071      70.3     628.5
e00600-e26270       0.033     0.046     0.075    0.013    0.042     182.3     628.5
e00650-e26270       0.026     0.039     0.067    0.013    0.040     130.6     628.5
e00700-e26270       0.268     0.107     0.317   -0.161    0.050      26.9     628.5
e00800-e26270      -0.003    -0.003    -0.005    0.000   -0.002       7.6     628.5
e00900-e26270      -0.010    -0.024    -0.003   -0.014    0.007     385.9     628.5
e01100-e26270       0.011     0.024     0.023    0.013    0.011       1.7     628.5
e01200-e26270      -0.018    -0.080    -0.009   -0.062    0.009      43.2     628.5
e01400-e26270       0.006     0.010     0.063    0.004    0.057     219.1     628.5
e01500-e26270      -0.004    -0.018    -0.007   -0.014   -0.003     905.2     628.5
e01700-e26270      -0.009    -0.010    -0.008   -0.001    0.001     579.2     628.5
e02000-e26270       0.976     0.948     0.979   -0.027    0.003     764.0     628.5
e02100-e26270      -0.038    -0.019    -0.031    0.019    0.007      38.0     628.5
e02300-e26270      -0.018    -0.028    -0.023   -0.010   -0.005      92.6     628.5
e02400-e26270      -0.005    -0.013    -0.008   -0.008   -0.003     495.3     628.5
e03150-e26270       0.015     0.038     0.029    0.022    0.014      11.1     628.5
e03210-e26270      -0.018    -0.008    -0.035    0.010   -0.018       9.7     628.5
e03220-e26270      -0.011     0.004    -0.018    0.015   -0.007       1.0     628.5
e03230-e26270      -0.014     0.000    -0.020    0.013   -0.006       4.2     628.5
e03240-e26270       0.384     0.387     0.452    0.003    0.067       8.1     628.5
e03270-e26270       0.179     0.230     0.174    0.052   -0.005      24.7     628.5
e03290-e26270       0.048     0.076     0.065    0.028    0.017       3.1     628.5
e03300-e26270       0.104     0.116     0.089    0.012   -0.015      19.0     628.5
e03400-e26270       0.001    -0.019     0.001   -0.020    0.000       0.5     628.5


# Distributional analysis of weighted variables against "true" puf

## CDF of file weights

<img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-11-1.png" width="1344" />



## CDFs of weighted values, continuous variables






```
##  [1] "DSI"    "EIC"    "fded"   "f2441"  "f6251"  "MARS"   "MIDR"   "n24"    "XTOT"   "e00200" "e00300" "e00400"
## [13] "e00600" "e00650" "e00700" "e00800" "e00900" "e01100" "e01200" "e01400" "e01500" "e01700" "e02000" "e02100"
## [25] "e02300" "e02400" "e03150" "e03210" "e03220" "e03230" "e03270" "e03240" "e03290" "e03300" "e03400" "e03500"
## [37] "e07240" "e07260" "e07300" "e07400" "e07600" "p08000" "e09700" "e09800" "e09900" "e11200" "e17500" "e18400"
## [49] "e18500" "e19200" "e19800" "e20100" "e20400" "p22250" "p23250" "e24515" "e24518" "e26270" "e27200" "e32800"
## [61] "e58990" "e62900" "e87521" "e87530" "s006"   "ftype"  "wt"
```

```
##  [1] "e00200" "e00300" "e00400" "e00600" "e00650" "e00700" "e00800" "e00900" "e01100" "e01200" "e01400" "e01500"
## [13] "e01700" "e02000" "e02100" "e02300" "e02400" "e03150" "e03210" "e03220" "e03230" "e03270" "e03240" "e03290"
## [25] "e03300" "e03400" "e03500" "e07240" "e07260" "e07300" "e07400" "e07600" "p08000" "e09700" "e09800" "e09900"
## [37] "e11200" "e17500" "e18400" "e18500" "e19200" "e19800" "e20100" "e20400" "p22250" "p23250" "e24515" "e24518"
## [49] "e26270" "e27200" "e32800" "e58990" "e62900" "e87521" "e87530"
```

<img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-1.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-2.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-3.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-4.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-5.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-6.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-7.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-8.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-9.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-10.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-11.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-12.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-13.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-14.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-15.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-16.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-17.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-18.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-19.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-20.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-21.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-22.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-23.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-24.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-25.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-26.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-27.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-28.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-29.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-30.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-31.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-32.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-33.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-34.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-35.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-36.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-37.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-38.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-39.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-40.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-41.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-42.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-43.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-44.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-45.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-46.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-47.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-48.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-49.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-50.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-51.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-52.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-53.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-54.png" width="1344" /><img src="D:\GOOGLE~1\OSPC\SYNPUF~1\EVAL_E~1/figure-html/unnamed-chunk-13-55.png" width="1344" />



# Run actual and synthesized data through Tax-Calculator















## Graph cumulative distribution of variables vs. AGI
### Wages






