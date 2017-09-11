***************************************************************************************************************************************;
***************************************************************************************************************************************;
*** Macro implementing PAVA (Pool Adjacent Violators Algorithm) to effect automatic binning. PAVA produces the solution to the 		***;
*** so-called Isotonic Regression Problem, wherein one seeks to find the best least-squares solution of a regression of one variable***;
*** against another, subject to the constraint that the estimate must be a non-increasing or non-decreasing function of the 			***;
*** independent variables.  This corresponds precisely to a Gini-maximization scheme whereby one construes the EDA as an ROC curve	***;
*** graphing cumulative goods and cumulative bads against one another.  The maximization itself consists of simply identifying the	***;
*** convex hull of the ROC curve.																												***;
*** Author: Trevis Litherland 																													***;
*** Date: Jan 13, 2016 																															***;
*** Version: Beta 1.0 																															***;
*** Description: Given a data set with a dependent target variable (binary or continuous), an input variable list, and an optional 	***;
***              weighting variable, dummy code and variable lists are produced for each qualifying variable. Three types of code 	***;
***              are produced: 																													***;
***              1) Linearized (WOE-based) code 																								***;
***              2) Usual 0/1 binned code 																										***;
***              3) 0/1 step-function code (overlapping intervals) 																			***;
*** Notes  																																		***;
***   1) Weighting variable is optional 																										***;
***   2) Parsing proceeds at 0.1% level, i.e., thousandths rather than percentiles 														***;
***   3) Missings are flagged separately as interval dummy variables but can be excluded from variable lists.							***;
***      For linearized variables, the missings are imputed either per observed value or, if no missings actually are present, per	***;
***      the overall average value																												***;
***   4) Output code and dummy variable lists are appended upon rerunning rather than overwritten.  									***;
***      Delete files manually before rerunning. 																								***;
***   5) Interval dummy variable counts per source variable are controlled by; 															***;
***      a) Maximal number of final (non-missing) intervals, 																				***;
***      b) Minimal qualifying volumes per standard 0/1 interval 																			***;
***      c) Minimal (bad) rate difference between consecutive standard 0/1 intervals 														***;
***   6) Only the interval and step-function code are subject to variable reduction.  The liniearized WOE code retains the full		***;
***      details of the non-collapsed intervals (up to 1,000 intervals in theory)															***;
**    7) The algorithm examines both potential increasing and decreasing solutions and identifies the better of the two as the		***;
***      canonical trend to go with																												***;
***   8) A folder to which all code and variable lists are output must be created prior to running the macro							***;
***   9) One can choose either logistic or linear for the linearization variable encodings												***;
***  10) The code below loads up some faux data for illustration purposes.																	***;
***************************************************************************************************************************************;

********************;
*** macro proper ***;
********************;

%macro intervalize;
		
	%* Step #1: Parse variable list -- only keeps numeric fields *;	

	%** Dummy data for input ordering of variables -- initializing fields only to avoid err0r in log window **;
	data dummy_order;
		format %model_vars 8.;
		array dummy(*) %model_vars;
		do i = 1 to dim(dummy);
			dummy(i) = 0;
		end;
		drop i;
	run;
	
	proc contents data=dummy_order noprint out=out_cont_dummy(keep=name varnum); 
	run;
	
	%** Actual data **;
	proc contents data=&input_data(keep=%model_vars) noprint out=out_cont_actual; 
	run;
	
	data n_out_cont_actual;
		set out_cont_actual;
		if type = 1 then output;
	run;
	
	%** Merging input list with those actually found on file and then sorting to obtain input ordering **;
	
	proc sort data=out_cont_dummy;		by name; run;
	proc sort data=n_out_cont_actual; 	by name; run;
	data n_out_cont;
		merge n_out_cont_actual(in=a) out_cont_dummy(in=b);
		by name;
		if a;
	run;
	proc sort data=n_out_cont; by varnum; run;
	
	%** Creating resulting macro variables; 
	data _null_;
		set n_out_cont end=last;
		if last then call symput('n_var_ct',_n_);
	run;
	
	data _null_;
		set n_out_cont;
		%do i = 1 %to &n_var_ct;
			if _n_ = &i then call symput("n_var_&i",name);			
		%end;
	run;
	

	%***************************************************************;

	%* Step #2: Breaking up data set to speed up processing;

	data 
		%do i = 1 %to &n_var_ct;
			input_x_&i(keep=&&n_var_&i &dep_var &wt_var)
		%end;
		;
		set &input_data;
	run;


	
/*
%do i = 1 %to &n_var_ct;
	title "data set for variable &i";
	proc means data=input_x_&i;
		var &&n_var_&i;
	run;
	
%end;
*/
	******************************************************************;
	**** overall counts and average ***;
	proc means data=&input_data noprint sumwgt vardef=WEIGHT;
		var &dep_var;
		output out=out_means sumwgt=total_volume mean=avg_with_missing;
		%if &wt_var NE %then %do;
			weight &wt_var;
		%end;
	run;
	
	data _null_;
		set out_means;
		call symput('TOT_VOL',total_volume);
		call symput('AVG_WITH_MISSING',avg_with_missing);
	run;
	


	
	%***************************************************************;
	%***************************************************************;

	%* Looping through all variables ***;

	%do i = 1 %to &n_var_ct;

		%***************************************************************;

		%* Step #3: Finding percentiles on nonmissing ;

		proc univariate data=input_x_&i noprint;
			where &&n_var_&i ne .;
			var &&n_var_&i;
			%if &wt_var NE %then %do;
				weight &wt_var;
			%end;
			output out=out_univ pctlpre=p pctlpts = 0 to 100 by 0.1;
		run;

/*proc print data=out_univ; run;	*/	
	
		%***************************************************************;

		%* Step #4: Parsing percentiles ;
		
		data temp(keep=break_point &dep_var &wt_var);
			set input_x_&i;
			if _n_ = 1 then set out_univ;
			
			%*array p_(0:100) p0--p100;
			
			if &&n_var_&i <= .Z then break_point = 999999999;
			/*
			%do j = 0 %to 100;
				else if &&n_var_&i <= p&j then break_point = p&j;
			%end;
			*/
			%do j = 0 %to 99;
				%if &j > 0 %then %do;
					else if &&n_var_&i <= p&j then break_point = p&j;
				%end;
				%do k = 1 %to 9;
					else if &&n_var_&i <= p&j._&k then break_point = p&j._&k;
				%end;
			%end;
			else if &&n_var_&i <= p100 then break_point = p100;
		run;

		
		%***************************************************************;

		%* Step #5: Getting stats by percentile;
		
		proc means data=temp noprint sumwgt vardef=WEIGHT;
			class break_point;
			var &dep_var;
			output out=out_means(where=(_TYPE_ = 1)) sumwgt=volume mean=average sum=sum_dep_var stddev=stand_dev;
			%if &wt_var NE %then %do;
				weight &wt_var;
			%end;
		run;
		
		
		%***************************************************************;

		%* Step #6: Running pool-adjacent violators algorithm (PAVA), forwards and backwards;
		data intervalize;
	
			set out_means(keep=break_point volume average sum_dep_var stand_dev) end=last;
			
%if &check_code = Y %then %do; check_code = 1; %end;
%else %do; check_code = 0; %end;			
			
			%* Running through data to put into array;
			array vol_(0:1001,2);
			array vol_cum_(0:1001,2);
			array avg_(0:1001,2);
			array sum_(0:1001,2);
			array s_x_cum_(0:1001,2);
			array s_xx_(0:1001,2);
			array s_xx_cum_(0:1001,2);
			array break_v_right_(0:1001,2);			
			array break_v_left_(0:1001,2);			
			
			vol_cum_(0,1) 	= 0;
			sum_(0,1)		= 0;
			s_x_cum_(0,1) 	= 0;
			s_xx_cum_(0,1) 	= 0;
			break_v_right_(0,1) = -999999999;
			break_v_left_(0,1)  = -999999999;
			
			vol_cum_(0,2) 	= 0;
			sum_(0,2)		= 0;
			s_x_cum_(0,2) 	= 0;
			s_xx_cum_(0,2) 	= 0;
			break_v_right_(0,2) = 999999999;
			break_v_left_(0,2)  = 999999999;
			
			
			r 				= _n_				;
			vol_(r,1) 		= volume			;
			vol_cum_(r,1)	= vol_cum_(r-1,1) + vol_(r,1);
			avg_(r,1) 		= average			;
			/*
			s_x_cum_(r,1)	= s_x_cum_(r-1,1) + vol_(r,1)*avg_(r,1);
			*/
			sum_(r,1)		= sum_dep_var;			
			s_x_cum_(r,1)	= s_x_cum_(r-1,1) + sum_(r,1);
			
			s_xx_(r,1) 		= volume*( max(0,stand_dev)**2 + average**2);
			s_xx_cum_(r,1)	= s_xx_cum_(r-1,1) + s_xx_(r,1);
			
			break_v_right_(r,1) = break_point;
			break_v_left_(r,1)  = break_v_right_(r-1,1);
			retain vol_: avg_: s_xx_: break_v_right_: break_v_left_: vol_cum_: sum_: s_x_cum_: s_xx_cum_:;
			
			last_missing = (break_point = 999999999);
			
			%******************************************************************;	
			%******************************************************************;	

			if last then do;
				
				if last_missing then n = r-1;
				else n = r;
				

				
				%*** reversed order copy - non-missings ***;
				do k = 1 to n;
					vol_(k,2) 		= vol_(n+1-k,1);
					vol_cum_(k,2)	= vol_cum_(k-1,2) + vol_(k,2);
					avg_(k,2) 		= avg_(n+1-k,1);
					
					sum_(k,2)		= sum_(n+1-k,1);
					/*
					s_x_cum_(k,2)	= s_x_cum_(k-1,2) + vol_(k,2)*avg_(k,2);
					*/
					s_x_cum_(k,2)	= s_x_cum_(k-1,2) + sum_(k,2);
					
					s_xx_(k,2) 		= s_xx_(n+1-k,1);	
					s_xx_cum_(k,2)	= s_xx_cum_(k-1,2) + s_xx_(k,2);
					
					break_v_right_(k,2)	= break_v_right_(n+1-k,1);
					break_v_left_(k,2)	= break_v_left_(n+1-k,1);

if check_code = 1 then file log;					
if check_code = 1 then put vol_(k,1)= avg_(k,1)= break_v_left_(k,1)= break_v_right_(k,1)=   vol_(k,2)= avg_(k,2)= break_v_left_(k,2)= break_v_right_(k,2)=;	
				
				end;
				
				if last_missing then do;
					vol_(r,2) 		= vol_(r,1);
					avg_(r,2) 		= avg_(r,1);
					sum_(r,2)		= sum_(r,1);
					s_xx_(r,2) 		= s_xx_(r,1);	
					*break_val_(r,2)	= break_val_(r,1);
				end;
				
				%******************************************************************;	
						
				%********* PAVA proper **********;
							
											
				array avg_bin_(0:1000,2)		;
				array vol_bin_(0:1000,2)		;
				array break_bin_(0:1000,2)	;
				array break_v_right_bin_(0:1000,2)	;
				array break_v_left_bin_(0:1000,2)	;
				array s_xx_bin_(0:1000,2)	;
				array num_bin_(2)			;
				array fit_(2)				;
				
				
				%******************************************************************;	
				
				do up_down = 1 to 2;
	if check_code = 1 then file log;		
	if check_code = 1 then put;				
					j = 0;
					
					avg_bin_(0,up_down) 	= -999999999; 
					vol_bin_(0,up_down) 	= 0; 
					s_xx_bin_(0,up_down) 	= 0;
					break_bin_(0,up_down)   = 0;
					fit_(up_down)			= 0;
					
					%******************************************************************;	
					
					do k = 1 to n;
						j = j + 1;
						avg_bin_(j,up_down) 	= avg_(k,up_down);
						vol_bin_(j,up_down) 	= vol_(k,up_down);
						s_xx_bin_(j,up_down)	= s_xx_(k,up_down);
						break_bin_(j,up_down)	= k;
						
						break_v_right_bin_(j,up_down) = break_v_right_(k,up_down);
						break_v_left_bin_(j,up_down)  = break_v_left_(k,up_down);
  if check_code = 1 then file log;						
  if check_code = 1 then put "Next try " up_down= k= j= avg_bin_(j,up_down)= vol_bin_(j,up_down)= break_bin_(j,up_down)=;		
  
						if avg_(k,up_down) > avg_bin_(j-1,up_down) then OK = 1;
						else OK = 0;
						
						%******************************************************************;	
	
						do while (OK = 0);
							j = j - 1;
														
							avg_bin_(j,up_down) 	= (s_x_cum_(break_bin_(j+1,up_down),up_down) - s_x_cum_(break_bin_(j-1,up_down),up_down))
							                        / (vol_cum_(break_bin_(j+1,up_down),up_down) - vol_cum_(break_bin_(j-1,up_down),up_down));							
							
							vol_bin_(j,up_down) 	= vol_cum_(break_bin_(j+1,up_down),up_down) - vol_cum_(break_bin_(j-1,up_down),up_down);
							s_xx_bin_(j,up_down)	= s_xx_cum_(break_bin_(j+1,up_down),up_down) - s_xx_cum_(break_bin_(j-1,up_down),up_down);						
							
							break_bin_(j,up_down)	= break_bin_(j+1,up_down);
							break_v_right_bin_(j,up_down) = max(break_v_right_bin_(j,up_down), break_v_right_bin_(j+1,up_down));
							break_v_left_bin_(j,up_down)  = min(break_v_left_bin_(j,up_down),  break_v_left_bin_(j+1,up_down)) ;
  if check_code = 1 then file log; 
  if check_code = 1 then put "  Pooling " OK= up_down= k= j= avg_bin_(j,up_down)= vol_bin_(j,up_down)= break_bin_(j,up_down)=;
							
							if avg_bin_(j,up_down) > avg_bin_(j-1,up_down) then OK = 1;						
						end;
						
					end;
					
					if last_missing then do;
						num_bin_(up_down) 		= j+1;
						avg_bin_(j+1,up_down) 	= avg_(r,1);
						vol_bin_(j+1,up_down) 	= vol_(r,1);						
						s_xx_bin_(j+1,up_down)	= s_xx_(r,1);
						break_bin_(j+1,up_down)	= r;
  if check_code = 1 then file log; 
  if check_code = 1 then put;
  if check_code = 1 then put "  Missing " up_down= k= num_bin_(up_down)= avg_bin_(j,up_down)= vol_bin_(j,up_down)= break_bin_(j,up_down)=;
					end;
					else do;
						num_bin_(up_down) = j;
					end;
					
				end;
				
				%******************************************************************;
				
				do up_down = 1 to 2;
  if check_code = 1 then file log; 															
  if check_code = 1 then put ; 
  if check_code = 1 then put 'a' 'avg_ vol_ sxx_ break_val_';
  if check_code = 1 then do;
  do j = 1 to n;
   	put @1 'b' avg_(j,up_down) @16 vol_(j,up_down) @31 s_xx_(j,up_down) @46 break_v_left_(j,up_down) @61 break_v_right_(j,up_down);					
  end;
end;
	if check_code = 1 then put "Variable #&i : &&n_var_&i"; 
	if check_code = 1 then put 'c' 'avg_bin_ vol_bin_ break_bin_ value at break_bin';	
					
					sum_vol = 0;
					sum_vol_x = 0;
					do j = 1 to num_bin_(up_down);
						put @1 avg_bin_(j,up_down) @16 vol_bin_(j,up_down) @31 break_bin_(j,up_down) 
						@46 break_v_left_bin_(j,up_down) @61 break_v_right_bin_(j,up_down) ;		

						sum_vol = sum_vol + vol_bin_(j,up_down);
						fit_(up_down)	= fit_(up_down) 
									+ s_xx_bin_(j,up_down)
									- vol_bin_(j,up_down)*avg_bin_(j,up_down)**2;	

					sum_vol_x = sum_vol_x + vol_bin_(j,up_down)*avg_bin_(j,up_down);
					end;

					%** RMS err0r **;
					fit_(up_down) 	= fit_(up_down)/sum_vol;
					avg_overall		= sum_vol_x/sum_vol;
					
					put;
					put "Fit (SSE)";
					put fit_(up_down)= sum_vol= sum_vol_x avg_overall;
				end;
			
				%** The best fit **;
			
				if fit_(1) < fit_(2) then best_fit = 1;
				else best_fit = 2;
				
if check_code = 1 then file log; 															
if check_code = 1 then put; 
if check_code = 1 then put  'd' "The best fit is " best_fit fit_(best_fit);  
			

				%******************************************************************************************************;
												
				%* Outputting (augmenting) linearized code files ;
														
				%*****************************************************************;
				
				
				n = num_bin_(best_fit);
if check_code = 1 then file log;				
if check_code = 1 then put 'e' n=  last_missing=;				
				
				%* will only use those source variables that had 2+ non-missing intervals **;
				if (n-last_missing) > 1 then do;
				
					format source_name dummy_name $32.;				
					source_name = trim("&&n_var_&i");
					
					%* linearized variable list flat file *;
					
					dummy_name = trim(source_name)||"_x";
					
					FILE "&code_dir./lin_var_list.sas" MOD;
					put dummy_name;
					
					%******************************************************************;			
					
					%* linearized variable list code file *;
					
					FILE "&code_dir./lin_code.sas" MOD;
					put;
									
					t = best_fit;
					
					%* missing value imputation *;
					
					if last_missing then do;
						band_avg = avg_bin_(n,t);
						n = n - 1;
					end;
					else band_avg = avg_overall;
					
					%if &reg_type = OLS %then %do;
						dummy_val = band_avg;
					%end;
					%else %if &reg_type = LOGISTIC %then %do;
						band_avg 	= min(max(band_avg,0.0001),0.9999);
						dummy_val 	= log( band_avg/(1-band_avg) );
					%end;			
					
					put "if " source_name " <= .Z then " dummy_name " = " dummy_val ";";
					
					%* non-missing values **;
					
					%* first band **;
					if t = 1 then do;
						right_break	= break_v_right_bin_(1,t);
						band_avg 	= avg_bin_(1,t);
					end;
					else do;
						right_break = break_v_right_bin_(n,t);
						band_avg 	= avg_bin_(n,t);
					end;
					
					%if &reg_type = OLS %then %do;
						dummy_val = band_avg;
					%end;
					%else %if &reg_type = LOGISTIC %then %do;
						band_avg 	= min(max(band_avg,0.0001),0.9999);
						dummy_val 	= log( band_avg/(1-band_avg) );
					%end;	 
					
					put "else if .Z < " source_name " <= " right_break " then " dummy_name " = " dummy_val ";";
					
					%* remaining bands **;
					if n > 1 then do;
						do j = 2 to n;
							if t = 1 then k = j;
							else k = n + 1 - j;							
						
							left_break 	= break_v_left_bin_(k,t);
							right_break = break_v_right_bin_(k,t);
					
							band_avg 	= avg_bin_(k,t);

							%if &reg_type = OLS %then %do;
								dummy_val = band_avg;
							%end;
							%else %if &reg_type = LOGISTIC %then %do;
								band_avg = min(max(band_avg,0.0001),0.9999);
								dummy_val = log( band_avg/(1-band_avg) );
							%end;	 
							
							if j < n then do;
								put "else if " left_break " < " source_name " <= " right_break " then " dummy_name " = " dummy_val ";";
							end;
							else do;
								put "else if " left_break " < " source_name " then " dummy_name " = " dummy_val ";";
							end;
						end;
					end;
								
				
				end; %* if n > 1 *;
				
				
				**********************************************************************;
				**********************************************************************;
				
				
				*** finding reduced intervals ***;
											
				*** initialize ***;
				t = best_fit;
				n = num_bin_(best_fit); **** added this puppy in ***;
				
				if last_missing then n_1 = n-1;
				else n_1 = n;
if check_code = 1 then file log;
if check_code = 1 then put 'f '  t= n= n_1=;	

				
				array prev_(0:1000);
				array next_(0:1000);
				array pct_vol_bin_(0:1001);
				prev_(0) = 0;
				next_(0) = 1;
				pct_vol_bin_(0) = 0;
				
				do i = 1 to n_1-1;
					prev_(i) = i - 1;
					next_(i) = i + 1;
				end;
				prev_(n_1) = n_1 - 1;
				next_(n_1) = n_1;
				
				do i = 1 to n_1;
					pct_vol_bin_(i) = 100*vol_bin_(i,t)/&TOT_VOL;
				end;
				
				
				*** finding initial parameters ***;
				num_int 		= n_1;
				min_vol  		= 9999999;
				min_vol_loc 	= 9999999;
				min_diff 		= 9999999;
				min_diff_loc 	= 9999999;
				
				do i = 1 to num_int;
if check_code = 1 then file log;
if check_code = 1 then put 'g ' num_int= i=;				
					if min_vol > pct_vol_bin_(i) then do;
						min_vol     = pct_vol_bin_(i);
						min_vol_loc = i;
					end;
					if i > 1 then do;
						if min_diff      > avg_bin_(i,t) - avg_bin_(i-1,t) then do;
							min_diff     = avg_bin_(i,t) - avg_bin_(i-1,t);
							min_diff_loc = i;
						end;
					end;
				end;

if check_code = 1 then file log;
if check_code = 1 then put 'h ' num_int= min_vol= min_vol_loc= min_diff= min_diff_loc=;	
			
				
				if num_int <= &num_int and min_vol >= &min_vol and min_diff >= &min_diff then do;
					OK = 1;
					j  = n_1;  *** fin_j = j after do while loop -- will not hit OK=0 criterion so just set j  to number of non-missing intervals ***; 
				end;
				else OK = 0;
				
				*** iterate until parameters are in compliance ***;
				do while (OK = 0);
				
					if      min_vol  < &min_vol  then j = min_vol_loc;
					else if min_diff < &min_diff then j = min_diff_loc;
					else                              j = min_vol_loc;

					*** if last one, then merge with one to left ***;
					*** otherwise, merge with one to right ***;
					if j = n_1 then do;
						avg_bin_(j,t) 	= (s_x_cum_(break_bin_(j,t),t) - s_x_cum_(break_bin_(prev_(prev_(j)),t),t))
							            / (vol_cum_(break_bin_(j,t),t) - vol_cum_(break_bin_(prev_(prev_(j)),t),t));							
							
						vol_bin_(j,t) 	= vol_cum_(break_bin_(j,t),t) - vol_cum_(break_bin_(prev_(prev_(j)),t),t);
						pct_vol_bin_(j) = 100*vol_bin_(j,t)/&TOT_VOL;
						
						s_xx_bin_(j,t)	= s_xx_cum_(break_bin_(j,t),t) - s_xx_cum_(break_bin_(prev_(prev_(j)),t),t);						
										
						break_v_right_bin_(j,t) = max(break_v_right_bin_(j,t), break_v_right_bin_(prev_(j),t));
						break_v_left_bin_(j,t)  = min(break_v_left_bin_(j,t),  break_v_left_bin_(prev_(j),t)) ;
												
						*** remove prev_(j) ***;
						next_(prev_(prev_(j))) = j;
						prev_(j) = prev_(prev_(j));
						
					end;
					
					else do;
						avg_bin_(next_(j),t) 	= (s_x_cum_(break_bin_(next_(j),t),t) - s_x_cum_(break_bin_(prev_(j),t),t))
							                    / (vol_cum_(break_bin_(next_(j),t),t) - vol_cum_(break_bin_(prev_(j),t),t));							
							
						vol_bin_(next_(j),t) 	= vol_cum_(break_bin_(next_(j),t),t) - vol_cum_(break_bin_(prev_(j),t),t);
						pct_vol_bin_(next_(j)) = 100*vol_bin_(next_(j),t)/&TOT_VOL;
						
						s_xx_bin_(next_(j),t)	= s_xx_cum_(break_bin_(next_(j),t),t) - s_xx_cum_(break_bin_(prev_(j),t),t);						
						
						*break_bin_(j,t)	= break_bin_(next_(j),t);
						break_v_right_bin_(next_(j),t) = max(break_v_right_bin_(j,t), break_v_right_bin_(next_(j),t));
						break_v_left_bin_(next_(j),t)  = min(break_v_left_bin_(j,t),  break_v_left_bin_(next_(j),t)) ;
						
						*** remove j ***;
						next_(prev_(j)) = next_(j);
						prev_(next_(j)) = prev_(j);
						
					end;
					
					*** decrement number of intervals ***;
					num_int = num_int - 1;
					
					*** check OK criteria ***;

					min_vol  		= 9999999;
					min_vol_loc 	= 9999999;
					min_diff 		= 9999999;
					min_diff_loc 	= 9999999;
					
					j = 0;
					do i = 1 to num_int;
						j = next_(j);
  if check_code = 1 then file log;
  if check_code = 1 then put 'i ' num_int= i= j= prev_(j)= ;
						if min_vol > pct_vol_bin_(j) then do;
							min_vol     = pct_vol_bin_(j);
							min_vol_loc = j;
						end;
						if i > 1 then do;
							if min_diff      > avg_bin_(j,t) - avg_bin_(prev_(j),t) then do;
								min_diff     = avg_bin_(j,t) - avg_bin_(prev_(j),t);
								min_diff_loc = j;
							end;
						end;
					end;
					
					if num_int <= &num_int and min_vol >= &min_vol and min_diff >= &min_diff then OK = 1;
					else OK = 0;

if check_code = 1 then file log;	
if check_code = 1 then k = 0;
if check_code = 1 then put;
if check_code = 1 then put 'j ' k= num_int= min_vol= min_vol_loc= min_diff= min_diff_loc= num_int=;
if check_code = 1 then do;
do while (k < num_int);
	k = next_(k);
	put 'k ' num_int= k= break_v_left_bin_(k,t)= break_v_right_bin_(k,t)= avg_bin_(k,t)= pct_vol_bin_(k)=;
end;	
end;
if check_code = 1 then put 'l ' num_int= k= break_v_left_bin_(k,t)= break_v_right_bin_(k,t)= avg_bin_(k,t)= pct_vol_bin_(k)=;						
											
				end;
				
				*** assign final number of intervals ***;
				fin_j = j;
		
		
if check_code = 1 then file log;
*FILE "&code_dir./junk.sas" MOD;	
if check_code = 1 then k = 0;
if check_code = 1 then put;
if check_code = 1 then put 'm ' num_int= k= min_vol= min_vol_loc= min_diff= min_diff_loc= num_int=;
if check_code = 1 then do;
do i = 1 to num_int;*
	k = next_(k);
	put @1 'n ' k @5 break_v_left_bin_(k,t) @15 break_v_right_bin_(k,t) @25 avg_bin_(k,t) 8.4 @35 pct_vol_bin_(k) 8.4 vol_bin_(k,t) 8.0;
end;	
end;	
				
				
			************************************************************************************************;
			
			*** outputting interval variables if at 2 non-missing intervals ***;
			
			if num_int > 1 then do;
if check_code = 1 then file log; 
if check_code = 1 then put 'x1';				
					format source_name dummy_name $32.;				
					source_name = left(trim("&&n_var_&i"));
					
					format dummy_root $28. suffix $4. dummy_name $32.;
					len_source_name = length(source_name);
					if len_source_name <= 28 then dummy_root = source_name;
					else                          dummy_root = left(trim("var_&i"));
					
					***********************************************;
					
					*** finding neutral interval for int code ***;
					neutral_diff     = 9999999;
					neutral_interval = 9999999;
if check_code = 1 then file log; 
if check_code = 1 then put 'x2 ' t=;							
					if t = 1 then j = next_(0);	
					else          j = fin_j;	
if check_code = 1 then file log; 
if check_code = 1 then put 'x3 ' t= j=;							
					i = 1;
					do while (1 <= i <= num_int);
						if abs(avg_bin_(j,t) - &AVG_WITH_MISSING) < neutral_diff then do;
							neutral_diff = abs(avg_bin_(j,t) - &AVG_WITH_MISSING);
							neutral_interval = j;
						end;
if check_code = 1 then file log; 
if check_code = 1 then put 'x4 ' t= i= j=;							
						
						if t = 1 then j = next_(j);
						else          j = prev_(j);
						i = i + 1;
					end;
if check_code = 1 then file log; 
if check_code = 1 then put 'x5 ' t= i= j=;							
					
					
					
					***********************************************;

					FILE "&code_dir./int_code.sas" MOD;
					put;
					FILE "&code_dir./step_code.sas" MOD;
					put;
					
					*** missing interval ***;
										
					
					%if &include_miss_intv = Y %then %do;
						dummy_name = left(trim(source_name))||"_z";
						
						FILE "&code_dir./int_var_list.sas" MOD;
						put dummy_name;
					
						FILE "&code_dir./int_code.sas" MOD;
						put "if " source_name " <= .Z then " dummy_name " = 1; else " dummy_name " = 0;";
						put "label " dummy_name " = '" dummy_name ":" source_name " = MISSING';";
						
						
						FILE "&code_dir./step_var_list.sas" MOD;
						put dummy_name;
					
						FILE "&code_dir./step_code.sas" MOD;
						put "if " source_name " <= .Z then " dummy_name " = 1; else " dummy_name " = 0;";
						put "label " dummy_name " = '" dummy_name ":" source_name " = MISSING';";
					%end;
						
						
						
					**************************************************************;	
if check_code = 1 then file log; 
if check_code = 1 then put 'x6 ';							
					
					if t = 1 then j = next_(0);	
					else          j = fin_j;	
					i = 1;
if check_code = 1 then file log; 
if check_code = 1 then put 'x7 ';							
					
					do while (0 <= i <= num_int);
																		
						if      i <  10 then suffix = trim('_00'||left(i));
						else if i < 100 then suffix = trim('_0'||left(i));
						else                 suffix = trim('_'||left(i));
						dummy_name = left(trim(source_name))||left(trim(suffix));
						
						*** interval variable list ***;
						FILE "&code_dir./int_var_list.sas" MOD;
						if j = neutral_interval then put "/*" dummy_name "*/";
						else put dummy_name;
						
						*** step variable list ***;
						FILE "&code_dir./step_var_list.sas" MOD;
						if i < num_int then do;
							put dummy_name;
						end;
						
						
						******************************************************************;			
if check_code = 1 then file log; 
if check_code = 1 then put 'x8 ';							
						
						left_break	= break_v_left_bin_(j,t);				
						right_break	= break_v_right_bin_(j,t);	
if check_code = 1 then file log; 
if check_code = 1 then put 'x9 ';							
						
						
						*** interval code file ***;
						FILE "&code_dir./int_code.sas" MOD;
												
						*** first dummy interval ***;
						if i = 1 then do;						
							put "if .Z < " source_name " <= " right_break " then " dummy_name " = 1; else " dummy_name " = 0;";
							put "label " dummy_name " = '" dummy_name ":" source_name " LOW - " right_break "';";
						end;
						
						*** last dummy interval ***;
						else if i = num_int then do;
							put "if " left_break " < " source_name " then " dummy_name " = 1; else " dummy_name " = 0;";
							put "label " dummy_name " = '" dummy_name ":" source_name " " left_break " <- HIGH ';";
						end;
						
						*** all others ***;
						else do;
							put "if " left_break " < " source_name " <= " right_break " then " dummy_name " = 1; else " dummy_name " = 0;";
							put "label " dummy_name " = '" dummy_name ":" source_name " " left_break " <- " right_break "';";
						end;						
						
						
						*** step code file ***;
						FILE "&code_dir./step_code.sas" MOD;
						if i < num_int then do;
						
							if t = 1 then do;
								put "if " right_break " < " source_name " then " dummy_name " = 1; else " dummy_name " = 0;";
								put "label " dummy_name " = '" dummy_name ":" source_name " " right_break " <- HIGH ';";
							end;
							
							else do;
								put "if .Z < " source_name " <= " right_break " then " dummy_name " = 1; else " dummy_name " = 0;";
								put "label " dummy_name " = '" dummy_name ":" source_name " LOW - " right_break "';";
							end;
							
						end;
						
if check_code = 1 then file log; 
if check_code = 1 then put 'x10';							
												
						*** increment ***;
						if t = 1 then j = next_(j);
						else          j = prev_(j);
if check_code = 1 then file log; 
if check_code = 1 then put 'x11';							
						i = i + 1;
									
									
					end;				
				
				end; %* if n > 1 *;
											
			end; %* if last then do;
			
		run;	
			
				
		%******************************************************************;
					

	%end; %* overall variable loop;

	** testing linearization ***;
	
	%if &test_coding = Y %then %do;
	
		data test_xxx;
			set &input_data(keep=
			&dep_var &wt_var
			%do i = 1 %to &n_var_ct;
				&&n_var_&i
			%end;
			);
		
			%inc "&code_dir./lin_code.sas";
		run;
		
		%do i = 1 %to &n_var_ct;
			%let dummy_name = %trim(&&n_var_&i)_x;
			title "checking means of linearized variables by class";
			proc means data=test_xxx vardef=WEIGHT;
				class &dummy_name;
				var &dep_var;
				%if &wt_var NE %then %do;
					weight &wt_var;
				%end;
			run;
			
				
			title "comparing correlations where source variable not missing";
			proc corr data=test_xxx;
				where &&n_var_&i ne .;
				var &dummy_name &&n_var_&i &dep_var;
				%if &wt_var NE %then %do;
					weight &wt_var;
				%end;
			run;
		%end;
	
		
		*** testing interval data ***;
		data test_xxx(drop=
			%do i = 1 %to &n_var_ct;
				&&n_var_&i
			%end;
			);
			set &input_data(keep=
			&dep_var &wt_var
			%do i = 1 %to &n_var_ct;
				&&n_var_&i
			%end;
			);
				
			
			%inc "&code_dir./int_code.sas";
						
			array check(*) 
			%do i = 1 %to &n_var_ct;
				%cmpres(%trim(&&n_var_&i):)
			%end;
			;
			
			*** converting flagged values to 0/1 just to check rates below ***;
			do i = 1 to dim(check);
				if check(i) = 0 then check(i) = .;
				else if &dep_var = '0' then check(i) = 0;
				else check(i) = 1;
			end;
			drop i;
						
			
		run;
		
		
		title "interval variable rates";
		proc means data=test_xxx;
			%if &wt_var NE %then %do;
				weight &wt_var;
			%end;
		run;
			
			
		
		*** testing step data ***;
		data test_xxx(drop=
			%do i = 1 %to &n_var_ct;
				&&n_var_&i
			%end;
			);
			set &input_data(keep=
			&dep_var &wt_var
			%do i = 1 %to &n_var_ct;
				&&n_var_&i
			%end;
			);
			
			%inc "&code_dir./step_code.sas";
											
			array check(*) 
			%do i = 1 %to &n_var_ct;
				%cmpres(%trim(&&n_var_&i):)
			%end;
			;
			
			*** converting flagged values to 0/1 just to check rates below ***;
			do i = 1 to dim(check);
				if check(i) = 0 then check(i) = .;
				else if &dep_var = '0' then check(i) = 0;
				else check(i) = 1;
			end;
			drop i;
			
			
									
		run;
		
		
		title "step variable rates";
		proc means data=test_xxx;
			%if &wt_var NE %then %do;
				weight &wt_var;
			%end;
		run;
				
			
				
	%end;	
	
	

%mend intervalize;



**************************************************************************************************************************************************;
**************************************************************************************************************************************************;
**************************************************************************************************************************************************;
**************************************************************************************************************************************************;

********************;
*** example call ***;
********************;

*** example full of ada variables ***;
%macro model_vars;
ANB_377
ANB_105_RE
ANB_106_RE
ANB_10_RE
ANB_121_RE
ANB_122_RE
ANB_12_RE
ANB_15_RE
ANB_1_RE
ANB_200_RE
ANB_201_RE
ANB_20_RE
ANB_230_RE
ANB_231_RE
ANB_235_RE
ANB_258_RE
ANB_259_RE
ANB_286_RE
ANB_287_RE
ANB_288_RE
ANB_2_RE
ANB_30_RE
ANB_319_RE
ANB_31_RE
ANB_320_RE
ANB_34_RE
ANB_377_RE
ANB_408_RE
ANB_41_RE
ANB_42_RE
ANB_43_RE
ANB_44_RE
ANB_45_RE
ANB_46_RE
ANB_49_RE
ANB_53_RE
ANB_54_RE
ANB_57_RE
ANB_60_RE
ANB_61_RE
ANB_63_RE
ANB_64_RE
ANB_66_RE
ANB_69_RE
ANB_6_RE
ANB_7_RE
ANB_84_RE
ANB_85_RE
ANB_88_RE
ANB_8_RE
ATT01_RE
ATT06_RE
ATT07_RE
ATT09_RE
ATT14_RE
ATT15_RE
ATT17_RE
ATT20_RE
ATT21_RE
ATT22_RE
ATT26_RE
ATT27_RE_MATT
ATT28_RE_MATT
ATT32_RE_MATT
ATT41_RE
ATT67_RE
ATT68_RE
ATT69_RE
ATT96_RE_MATT
ATT97_RE_MATT
BALMAX_RE
BALMIN_RE
BANKTOT_RE
BCHLRNM_RE
BCHLRTBL_RE
BCHLRTCL_RE
BRTB_RE
CA316
CA362
CA131_RE
CA132_RE
CA135_RE
CA136_RE
CA138_RE
CA139_RE
CA182_RE
CA190_RE
CA230_RE
CA231_RE
CA236_RE
CA241_RE
CA26_RE
CA284_RE
CA286_RE
CA292_RE
CA293_RE
CA308_RE
CA30_RE
CA312_RE
CA313_RE
CA314_RE
CA317_RE
CA318_RE
CA31_RE
CA320_RE
CA320_RE_a6m
CA321_RE
CA322_RE
CA324_RE
CA325_RE
CA327_RE
CA32_RE
CA332_RE
CA333_RE
CA334_RE
CA337_RE
CA339_RE
CA340_RE
CA341_RE
CA342_RE
CA344_RE
CA347_RE
CA349_RE
CA351_RE
CA356_RE
CA357_RE
CA358_RE
CA359_RE
CA35_RE
CA362_RE
CA363_RE
CA368_RE
CA36_RE
CA371_RE
CA374_RE
CA3_RE
CA4_RE
CA77_RE
CA78_RE
CA79_RE
CA81_RE
CA82_RE
CA84_RE
CA86_RE
CA88_RE
CLMIN_RE
CMI01_RE
CMI03_RE
CMI05_RE
CMI07_RE
CMI08_RE
CMI11_RE
CMI12_RE
CMI13_RE
HBALOPBK_RE
HD_DM3_RE
HEFLAG_RE_Y
HUTB_RE
INQ_5D_RE
LUTB_RE
MBANKOP_RE
OBCRVTL_RE
PROPEN
PROPEN_RE
VALMMPOB_RE
VALMMPOJ_RE
VALMMPRI_RE
VALMMPRV_RE
VALMMPTB_RE
VAL_14_RE
VAL_22_RE
VAL_26_RE
VAL_34_RE
VAL_38_RE
VAL_46_RE
VAL_49_RE
VAL_50_RE
VAL_5_RE
VAL_A11_RE
VAL_A21_RE
VAL_A22_RE
VAL_A_RE
VAL_B3_RE
VAL_BK_RE
VAL_B_RE
VAL_C10_RE
VAL_C2_RE
VAL_C3NA_RE
VAL_C3_RE
VAL_C4NA_RE
VAL_CG1_RE
VAL_C_RE
VAL_D_RE
VAL_E1_RE
VAL_E2_RE
VAL_E3_RE
VAL_F_RE
VAL_G5_RE
VAL_G6_RE
VAL_G7_RE
VAL_G_RE
VAL_HCLO_RE
VAL_HCT_RE
VAL_HLITB_RE
VAL_H_RE
VAL_ICLO_RE
VAL_ICT_RE
VAL_ITRM1_RE
VAL_I_RE
VAL_J_RE
VAL_K1_RE
VAL_KNEW
VAL_K_RE
VAL_L1_RE
VAL_L2_RE
VAL_L3_RE
VAL_L_RE
VAL_M2_RE
VAL_M77_RE
VAL_M79_RE
VAL_M7_RE
VAL_MAX_CURPAY
VAL_MCBAL
VAL_MCLO_RE
VAL_MCT_RE
VAL_MMP_RE
VAL_MTQ_RE
VAL_M_RE
VAL_N_RE
VAL_O_RE
VAL_P10_RE
VAL_P11_RE
VAL_P12_RE
VAL_P13_RE
VAL_P1_RE
VAL_P2_RE
VAL_P3_RE
VAL_P4_RE
VAL_P5_RE
VAL_P6_RE
VAL_P7_RE
VAL_P8_RE
VAL_P_RE
VAL_RC3NA_RE
VAL_RC3_RE
VAL_RFCLO_RE
VAL_RFNCT
VAL_RFNCT_RE
VAL_S3_RE
VAL_S6_RE
VAL_SLB_RE
VAL_SL_RE
VAL_T_RE
VAL_UT1
VAL_UT1_RE
VAL_UT2_RE
VAL_UT3_RE
VAL_UT4_RE
VAL_X2_RE
VAL_X3_RE
VAL_Z10_RE
VAL_Z12_RE
VAL_Z13_RE
VAL_Z16_RE
VAL_Z17_RE
VAL_Z18_RE
VAL_Z1_RE
VAL_Z21_RE
VAL_Z5_RE
VAL_Z6_RE
VAL_Z7_RE
VAL_Z8_RE
apr
ax_nabc13038_totals_score_d
ax_nabc13038_totals_st
ax_nabc13038_totals_seg_1
ax_nabc13038_totals_seg_2
ax_nabc13038_totals_seg_3
ax_nabc13039_revbal_score_d
ax_nabc13039_revbal_st
ax_nabc13039_revbal_seg_1
ax_nabc13039_revbal_seg_2
ax_nabc13039_revbal_seg_3
ax_nabc13040_prbal_score_d
ax_nabc13040_prbal_st
ax_nabc13040_prbal_seg_1
ax_nabc13040_prbal_seg_2
ax_nabc13040_prbal_seg_3
ax_nabc13040_prbal_seg_4
ax_nabc13041_frbal_score_d
ax_nabc13041_frbal_st
ax_nabc13041_frbal_seg_1
ax_nabc13041_frbal_seg_2
ax_nabc13041_frbal_seg_3
ax_nabc13042_hrbalpb_score
ax_nabc13042_hrbalpb_st
ax_nabc13044_hrbal_score_d
ax_nabc13044_hrbal_st
ax_nabc13044_hrbal_seg_1
ax_nabc13044_hrbal_seg_2
ax_nabc13044_hrbal_seg_3
ax_nabc13045_crprob_score
ax_nabc13045_crprob_st
ax_nabc13046_coaprob_score
ax_nabc13046_coaprob_st
ax_nabc13047_cooprob_score
ax_nabc13047_cooprob_st
ax_nabc13048_orprob_score
ax_nabc13048_orprob_st
ax_nabc13049_nrprob_score
ax_nabc13049_nrprob_st
ax_nabc13050_crs_score_d
ax_nabc13050_crs_st
ax_nabc13050_crs_seg_1
ax_nabc13050_crs_seg_2
ax_nabc13050_crs_seg_3
ax_nabc13051_coas_score_d
ax_nabc13051_coas_st
ax_nabc13051_coas_seg_1
ax_nabc13051_coas_seg_2
ax_nabc13051_coas_seg_3
ax_nabc13052_coos_score_d
ax_nabc13052_coos_st
ax_nabc13052_coos_seg_1
ax_nabc13052_coos_seg_2
ax_nabc13052_coos_seg_3
ax_nabc13053_ors_score_d
ax_nabc13053_ors_st
ax_nabc13053_ors_seg_1
ax_nabc13053_ors_seg_2
ax_nabc13053_ors_seg_3
ax_nabc13054_nrs_score_d
ax_nabc13054_nrs_st
ax_nabc13054_nrs_seg_1
ax_nabc13054_nrs_seg_2
ax_nabc13054_nrs_seg_3
ax_nabc13054_nrs_seg_4
ax_nabc13055_proapr_score_d
ax_nabc13055_proapr_st
ax_nabc13055_proapr_seg_1
ax_nabc13055_proapr_seg_2
ax_nabc13055_proapr_seg_3
ax_nabc13056_frapr_score_d
ax_nabc13056_frapr_st
ax_nabc13056_frapr_seg_1
ax_nabc13056_frapr_seg_2
ax_nabc13056_frapr_seg_3
ax_nabc13056_frapr_seg_4
ax_nabc13059_es_score_d
ax_nabc13059_es_st
ax_nabc13059_es_seg_1
ax_nabc13059_es_seg_3
ax_nabc13059_es_segg_2
ax_nabc13060_ts_score_d
ax_nabc13060_ts_st
ax_nabc13060_ts_seg_1
ax_nabc13060_ts_seg_2
ax_nabc13060_ts_seg_3
ax_nabc13061_tsprob_score
ax_nabc13061_tsprob_st
ax_nabc13062_os_score_d
ax_nabc13062_os_st
ax_nabc13062_os_seg_1
ax_nabc13062_os_seg_2
ax_nabc13062_os_seg_3
ax_nabc13063_osprob_score
ax_nabc13063_osprob_st
ax_nabc13064_revenue_score_d
ax_nabc13064_revenue_st
ax_nabc13064_revenue_seg_1
ax_nabc13064_revenue_seg_2
ax_nabc13064_revenue_seg_3
ax_nps_dc_18_xmld
ax_nps_dn_18_xmld
ax_nps_sv_18_xmld
bf_ca322_re_a6m
bf_citiusa9
bf_ecmtcor_re
bf_ecmtcore
bf_iheflag_re_Y
bf_mbankop_re_a6m
bf_mortflag_re_Y
bf_onestutil_re
bf_trgbc_re_BC
bf_trgnc_re_NC
bf_trgnm_re_NM
bf_trgnpsl_re_Y
bf_twondutil_re
bf_v_hcbal_re
bf_v_hcdt_re
bf_v_hobal_re
bf_v_hodt_re
bf_v_icbal_re
bf_v_icdt_re
bf_v_iobal_re
bf_v_iodt_re
bf_v_mcbal_re
bf_v_mcdt_re
bf_v_mobal_re
bf_v_modt_re
bf_v_rfcbal_re
bf_v_rfcdt_re
bf_v_rfobal_re
bf_v_rfodt_re
bf_valc3x_re
bf_valc4x_re
bf_valmmpop_re
bf_valrx_re
bf_valuep13
bf_valuerc6_re
bf_valz1
bf_valz2
bf_valz3
bf_valz4
bf_valz5
bf_valz6
bf_valz12
bf_valz13
bf_valz14
bf_valz15
bf_valz16
bf_valz17
bf_valz18
bf_valz19
bf_valz21
bt_dur 
%mend model_vars;



**************************************************************************************************************************************************;


*** example list for faux data only ***;



*** faux dataset creation for illustration only ***;



*** obtaining 50-50 weighting variable on model development data ***;

	
	
/*
title "checking 50-50 weighting";
proc freq data=faux_data;	where ransel < 500; tables bad/; weight wt_50_50;		run;
*/

**************************************************************************************************************************************************;


*** typical model_data creation from input data ***;

options set=SAS_HADOOP_JAR_PATH='/opt/sas/hadoop_jars';

***** BEGIN user data preparation (modify and customize as needed) *****;
libname perm hadoop server='adappsrku005.analytics.us.equifax.com' db="citi_ndt_poc_db" hdfs_tempdir='/user/cambrian/proj_ops/citi_ndt_poc_268158';

%include '/sas_cambrian/268158_citi_ndt_poc/pgms/var_prep_macros.sas';
data one;
	set perm.acq_simp_dev_data_feb14_v1_p(drop=%drop_immediately_vars mcell pr_name response_status);;
	rename %rename_vars;;
run;

data model_data;
	set one;
	%valid_var_range;
	%char_var_decoding;
	%var_labels;
	drop %drop_char_vars;;
	
	popwgt=1;
	if resp = 0 then gbwgt=1;
	else if resp = 1 then gbwgt = 1172;
	
	bad = resp;
	good=1-bad;
	
	if ranuni(4321) < 0.5;
	wgt = 2;
run;


title "checking weight";
proc freq data=model_data;
	tables resp /missing;
	weight wgt;
run;


**************************************************************************************************************************************************;
**************************************************************************************************************************************************;


*********************;
*** call to macro ***;
*********************;

%let input_data = model_data;	/* source data set 		*/
%let dep_var   	= resp;			/* dependent variable 	*/
%let wt_var		= wgt;		/* weighting variable 	*/



%let code_dir = %str(/sas_cambrian/268158_citi_ndt_poc/pgms/09_auto_binning/bin_code);	/* location of binned and linearized dummy variables, along with respective varlists */

%let reg_type = LOGISTIC;		/* =OLS for linear regression, =LOGISTIC for logistic regression (i.e., probabilities transformed to log-odds) */
%let num_int  =  10;				/* maximum number of non-missing binned dummy variables per source variable (includes neutral interval) **/
%let min_vol  =  1;				/* minimum % of total weighted volume that each standard 0/1 interval must contain **/
%let min_diff =  0.0001;    	    /* minimum difference in average target values that each standard 0/1 interval must maintain ***/

%let include_miss_intv = N;		/* = Y if missing indicator variables desired in binned output */
%let test_coding = N;		    /* = Y if we want to check that the bins are as advertised -- best practice is to use just a few variables */

%let check_code = N;			/* = Y for debugging (developer code) */


**************************************************************************************************************************************************;
**************************************************************************************************************************************************;


options mprint;
%intervalize;

endsas;

**************************************************************************************************************************************************;
**************************************************************************************************************************************************;

*** example of extra assessment of code ***;



endsas;



**************************************************************************************************************************************************;
**************************************************************************************************************************************************;


