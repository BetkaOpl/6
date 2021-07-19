;=================================================================================================================================================
; IDL procedures
;=================================================================================================================================================
;
;-------------------------------------------------------------------------------------------------------------------------------------------------
; Starting program 
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro StartTop, wWidget
  print, 'Start'
  
end

;-------------------------------------------------------------------------------------------------------------------------------------------------
; Ending program
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro EndTop, wWidget
  print, 'End'
  
end

;-------------------------------------------------------------------------------------------------------------------------------------------------
; Empty procedure autoloading
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro procedures
end

;-------------------------------------------------------------------------------------------------------------------------------------------------
; Exit program procedure
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro ExitMyApp, Event
  widget_control, event.top, /destroy
  
end

;-------------------------------------------------------------------------------------------------------------------------------------------------
; Help
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro HelpWindow, Event
  common CB_size_sc, size_sc

  wBase = WIDGET_BASE(UNAME='wBase', XOFFSET = 0.2 * size_sc[0], YOFFSET = 0.2 * size_sc[1] $
    , SCR_XSIZE = 750, SCR_YSIZE = 0.4 * size_sc[1],TITLE='Help', /SCROLL)

  ; New line
  m = String(13B)
  help_text = 'This program enables us to study asymmetry of spectral line profiles with the bisector method.' + m + m + 'Instructions:' $
    + m + m + '1) Press "Menu" and load data with spectral line profiles. Additionally, you can add data files or remove them.' + m + m $
    + '2) Double-click on a file to plot an individual spectral line profile. ' $
    + 'The table with settings appears to confirm default values or change them.' + m + m $
    + '3) Click on "Calculate bisector" to add bisector to the figure.' + m + m $
    + '4) To compute the bisector velocity span, choose the top part of the profile near the wings and the bottom part ' $
    + 'close to the kernel.' + m + m $
    + '5) Click on "Calculate BVS" to display a graph of intensity with respect to the shift in wavelength based on the Doppler effect.' + m $
    + '    The calculated BVS is displayed below. ' + m + m $
    + '6) To compute BVS of all data files, click on "Compute BVS for all files" and set required values.' + m + m $
    + '7) Load the corresponding file with radial velocities using the "Radial velocities" menu.' + m + m $
    + '8) To study the relationship between BVS and RVs, compute the correlation using the "Correlation" menu.'
  Label1 = Widget_Label(wBase, value =  help_text $
    , XOFFSET = 0.003 * size_sc[0], YOFFSET = 0.002 * size_sc[1] $
    , XSIZE = 650, YSIZE = 0.39 * size_sc[1], FONT = fon, frame = 0)

  WIDGET_CONTROL, wBase, /REALIZE

end

;-------------------------------------------------------------------------------------------------------------------------------------------------
; Setting widget
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro Settings, Event
  common CB_size_sc, size_sc
  common WID_CB, WID_BASE_0
  common CB_droplist, Drop_LIST_0, Drop_LIST_1, setbase, selected_index0, selected_index1, selected_index2, droplistValues0
  common CB_Drop_LIST_2, Drop_LIST_2
  common CB_drop, droplistValues1, droplistValues2
  selected_index1 = 0

  setBase = WIDGET_BASE(GROUP_LEADER = WID_BASE_0, UNAME='setBase', XOFFSET = 0.2 * size_sc[0], YOFFSET = 0.2 * size_sc[1] $
    , SCR_XSIZE = 580, SCR_YSIZE = 0.3 * size_sc[1], TITLE='Settings')

  ; New line
  m = String(13B)
  help_text = 'Check the range of intensities I1 (bottom) and I2 (top) for which bisectors will be computed.' + m + m $
    + 'The intensities are computed as follows:' + m + m $
    + 'I1 = I0 + C1 * (IC - I0)' + m $
    + 'I2 = IC - C2 * (IC - I0), ' + m + m $
    + 'where I0 stads for the minimum intensity of the profile line; IC, for continuum; C1 and C2, for coefficients in percentages.' + m + m $
  + 'You can set the coefficients below and the step in intensities as well. After setting of values, double-click on a file.' + m + m $
    + '   C1 (%)           C2 (%)           Step in I'

  Label1 = Widget_Label(setBase, value =  help_text $
    , XOFFSET = 0.002 * size_sc[0], YOFFSET = 0.002 * size_sc[1] $
    , XSIZE = 570, YSIZE = 160, FONT = fon, frame = 0, UNAME = 'Label1')

  dropListValues0 = ['4', '2', '6', '8', '10', '12', '14', '16', '18', '20', '22', '24', '26', '28', '30', '32', '34', '36' $
    , '38', '40', '42', '44', '46', '48', '50']

  Drop_LIST_0 = Widget_Droplist(setBase, value = dropListValues0, uvalue = dropListValues0 $
    , XOFFSET = 0.005 * size_sc[0], YOFFSET = 160 $
    , XSIZE = 53, YSIZE = 20, FONT = fon, frame = 1, UNAME = 'Drop_LIST_0')

  dropListValues1 = ['4', '2', '6', '8', '10', '12', '14', '16', '18', '20', '22', '24', '26', '28', '30', '32', '34', '36' $
    , '38', '40', '42', '44', '46', '48', '50']

  Drop_LIST_1 = Widget_Droplist(setBase, value = dropListValues1  $
    , XOFFSET = 0.005 * size_sc[0] + 65, YOFFSET = 160 $
    , XSIZE = 53, YSIZE = 20, FONT = fon, frame = 1, UNAME = 'Drop_LIST_1')

  dropListValues2 = ['0.001', '0.002', '0.003', '0.004', '0.005']

  Drop_LIST_2 = Widget_Droplist(setBase, value = dropListValues2  $
    , XOFFSET = 0.005 * size_sc[0] + 65 + 65, YOFFSET = 160 $
    , XSIZE = 59, YSIZE = 20, FONT = fon, frame = 1, UNAME = 'Drop_LIST_2')

  OK = Widget_Button(setBase, UNAME='OK', VALUE='OK', FONT = fon $
    , XOFFSET = 0.34 * size_sc[0], YOFFSET = 160 $
    , XSIZE = 60, YSIZE = 20)

  WIDGET_CONTROL, setBase, /REALIZE
  XManager, 'Settings', setBase, event_handler='Settings_ev'

end

;-------------------------------------------------------------------------------------------------------------------------------------------------
; Event - settings window
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro Settings_ev, Event

  if Widget_Info(Event.id, FIND_BY_UNAME='OK') eq Event.id then begin
    if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      OK, Event
  end

end

;-------------------------------------------------------------------------------------------------------------------------------------------------
; Ok for the settings window
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro OK, Event
  common CB_droplist, Drop_LIST_0, Drop_LIST_1, setbase, selected_index0, selected_index1, selected_index2, droplistValues0
  common CB_Drop_LIST_2, Drop_LIST_2
  common CB_drop, droplistValues1, droplistValues2
  common CB_values, coef1, coef2
  common CB_step, step

  ; Determine chosen strings from lists
  selected_index0 = Widget_Info(Drop_LIST_0, /DropList_Select)
  coef1 = droplistValues0[selected_index0]

  selected_index1 = Widget_Info(Drop_LIST_1, /DropList_Select)
  coef2 = droplistValues1[selected_index1]

  selected_index2 = Widget_Info(Drop_LIST_2, /DropList_Select)
  step = droplistValues2[selected_index2]

  WIDGET_CONTROL, setBase, /destroy

end

;-------------------------------------------------------------------------------------------------------------------------------------------------
; Loading data procedure - spectral line profiles
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro LoadMyFiles, Event
  common CB_shortname, shortname       ; name of files
  common CB_id_list_0, id_list_0       ; list of files
  common CB_path, path                 ; path of files
  
  ; Creating of a template
  ;file = dialog_pickfile()
  ;template = ascii_template(file)
  ;save,template,filename='template.sav'
  
  ; Loading data according to 'template.sav' - data starts on the 3rd row
  ; 
  restore,'template.sav'
  files = dialog_pickfile(/multiple_files, Get_Path = thePath) ; choose multiple files
  
  shortname = StrMid(files, StrLen(thePath))                   ; store only the name of the file
  path = replicate(thePath, n_elements(files))                 ; create list of paths
  i_sort = SORT(shortname)                                     ; sort by alphabet
  widget_control, id_list_0, set_value = shortname[i_sort]     ; set sorted names of files into the list
  
  ; Procedure for settings intensities of a bisector and its step
  Settings  
  
end

;-------------------------------------------------------------------------------------------------------------------------------------------------
; Adding data procedure
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro AddFile, Event
  common CB_shortname, shortname
  common CB_id_list_0, id_list_0 
  common CB_path, path
  
  restore,'template.sav'
  files = dialog_pickfile(/multiple_files, Get_Path = thePath) ; choose multiple files

  shortname_add = StrMid(files, StrLen(thePath) ) ; store only the name of the file
  sh = [shortname, shortname_add]
  
  ; Preventing adding one file twice
  shortname = sh[uniq(sh, sort(sh))]
  ai_sort = SORT(shortname)
  widget_control, id_list_0, set_value = shortname[ai_sort]
  path = [path, replicate(thePath, n_elements(files))]
    
end

;-------------------------------------------------------------------------------------------------------------------------------------------------
; Removing data procedure - for the highlighted file
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro RemoveFile, event, highlighted_item
  common CB_shortname, shortname
  
  if n_elements(shortname) eq 1 then begin
    shortname = strarr(1)
  endif else begin
  shortname2 = strarr(n_elements(shortname) - 1)
  j = 0
  for i = 0, n_elements(shortname) - 1 do begin 
    if i eq highlighted_item then continue
    shortname2[j] = shortname[i]
    j++
  endfor
  shortname = shortname2
  endelse
  
end
 
;-------------------------------------------------------------------------------------------------------------------------------------------------
; Opening data procedure
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro OpenFile, Event, item_ID
  common CB_path, path
  common CB_shortname, shortname
  common CB_minimum, minimum
  common CB_maximum, maximum
  common CB_BH, BH
  common CB_intensity, IC, I0, I1, I2
  common CB_step, step
  common CB_data, wavelength, intensity
  common CB_Imin, I_min
  common CB_ind_min_I, ind_min_I
  common CB_I, I
  common CB_sl, SLIDER1, SLIDER2
  common CB_ev, wWidget
  common CB_xx, xx
  common CB_x_ran, x_ran
  common CB_ran_I, ran_I
  common CB_flat_files, flat_files
  common CB_ID, data_ID

  ; Load template for data
  restore, 'template.sav'
  home = path[item_ID] + shortname[item_ID]
  data = read_ascii(home, template = template)

  ; Find minimum and maximum of the intensity
  minimum = min(data.FIELD2)
  maximum = max(data.FIELD2)
  
  ; List for files that are suspicious for being flat
  flat_files = LIST()
  
  ; Find index including the minimum of the profile
  ind_min_I = where(data.FIELD2 EQ minimum)
  Emin_I = N_ELEMENTS(ind_min_I)
  
  ; If the profile is flat, the last value is taken into account
  if Emin_I ne 1 then begin
    ind_min_I = ind_min_I[-1]
    file_e = item_ID
    flat_files.Add, file_e
  endif

  xx = data.FIELD1
  
  file_sh = STRMID(shortname[item_ID], 0, 3)

  ; x_ran = 700 for CAT, 200 for FER, 80 for HER, 30 for RET
  if file_sh EQ 'occ' then begin
    x_ran = 700 
  endif else begin
    if file_sh EQ 'fer' then begin
      x_ran = 200
    endif else begin
      if file_sh EQ 'her' then begin
        x_ran = 80
      endif else begin
        if file_sh EQ 'omc' then begin
          x_ran = 30
        endif
      endelse
    endelse
  endelse

  wavelength = xx[ind_min_I - x_ran : ind_min_I + x_ran] / 10.     ; in nanometers

  intensity  = data.FIELD2[ind_min_I - x_ran : ind_min_I + x_ran] 

  ; The index of the minimum
  I_min = where((intensity EQ minimum))
  I_min = I_min[-1]
  I0 = minimum
  IC = 1              ; intensity of continuum
  ran_I = IC - I0     ; range of intensity
   
  ; Set interval of intensities where bisector will be computed
  BisectorI
  
  ; Set sliders for each file 
  widget_control, widget_info(wWidget, find_by_uname='SLIDER1'), set_value = [10, 0, 50]         ; [value, min, max]
  widget_control, widget_info(wWidget, find_by_uname='SLIDER2'), set_value = [10, 0, 50]
  
  ; Saving item_ID for later Plotting
  data_ID = item_ID
  
end

;-------------------------------------------------------------------------------------------------------------------------------------------------
; Error for files that are quite flat
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro OpenErrors, Event, item_ID
  common CB_shortname, shortname
  common CB_ev, wWidget
  common CB_flat_files, flat_files
  
  ; Restart displaying result after opening a new file
  widget_control, widget_info(wWidget, find_by_uname='BVS'), set_value = 'BVS = ?'
  
  if N_ELEMENTS(flat_files) ne 0 then begin
    File_list_er = shortname(flat_files.toArray())
    wd = widget_info(wWidget, find_by_uname='WID_DRAW_ERR')
    if wd gt 0 then begin
      text_we = 'Check the appopriacy of using this too flat data file:'
      widget_control, wd, get_value = idraw
      idraw.select
      t_err_I = TEXT(0.01, 0.67, text_we, 'r', /RELATIVE, FONT_NAME = fon $
        , FONT_SIZE = 13, FONT_STYLE = "bf", HIDE = 0)
      err_files = TEXT(0.01, 0.47, File_list_er, 'r', /RELATIVE, FONT_NAME = fon $
        , FONT_SIZE = 13, FONT_STYLE = "bf", HIDE = 0)
      WAIT, 1.5
      idraw.erase
    endif
  endif

end

;-------------------------------------------------------------------------------------------------------------------------------------------------
; Loading data procedure - radial velocities
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro LoadRV, Event
  common CB_ev, wWidget
  common CB_RV, RV
  common CB_Number_file, NUmber_file

  ; Load data - RV and numbers from
  restore,'template_RV.sav'
  file = dialog_pickfile()
  data = read_ascii(file, template = template)
  RV = data.FIELD4
  Number_file = data.FIELD1

  ; Display information about loading data
  wd = widget_info(event.top, find_by_uname='WID_DRAW_ERR')
  if wd gt 0 then begin
    ; Obtain the window index
    widget_control, wd, get_value = idraw
    ; Set the new widget to be the current graphics window
    idraw.select
    ; No overplotting
    idraw.erase
    ; Plot text
    RV_loaded = TEXT(0.01, 0.7, 'File has been loaded.', 'navy', /RELATIVE, FONT_NAME = fon $
      , FONT_SIZE = 13, FONT_STYLE = "bf", HIDE = 0)
    ; The printed text will be erased in 1.5 seconds
    WAIT, 1.5
    idraw.erase
  endif

end

;-------------------------------------------------------------------------------------------------------------------------------------------------
; Saving I0 of all files procedure
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro SaveI0
  common CB_intensity, IC, I0, I1, I2
  common CB_I0_All, I0_all
  
  I0_all.Add, I0
  
end

;-------------------------------------------------------------------------------------------------------------------------------------------------
; Computing intensity range for bisectors 
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro BisectorI, Event, item_ID
  common CB_BH, BH
  common CB_intensity, IC, I0, I1, I2
  common CB_step, step
  common CB_I, I
  common CB_values, coef1, coef2
  common CB_step, step
  common CB_ran_I, ran_I

  ; Generate intensity range for determining bisector line
  I1 = I0 + FLOAT(coef1*0.01) * ran_I  ; lower bound of the bisector line
  I2 = IC - FLOAT(coef2*0.01) * ran_I  ; upper bound of the bisector line
  BH = I1 + (I2 - I1)/2           ; center of intensity of a bisector
  I = [I1 : I2 : FLOAT(step)]     ;
  
end

;-------------------------------------------------------------------------------------------------------------------------------------------------
; Plotting spectral line profile
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro PlottingData, Event, item_ID
  common CB_minimum, minimum
  common CB_maximum, maximum
  common CB_intensity, IC, I0, I1, I2
  common CB_data, wavelength, intensity
  common CB_ind_min_I, ind_min_I
  common CB_ran, ran1_x, ran2_x
  common CB_ev, wWidget
  common CB_ID, data_ID
  common CB_xx, xx
  common CB_x_ran, x_ran
  common CB_shortname, shortname
  common CB_p, p
  
  ; Hide table with results
  widget_control, widget_info(wWidget, find_by_uname='WID_Tab'), MAP = 0
  
  wd = widget_info(wWidget, find_by_uname='WID_DRAW_0')
  if wd gt 0 then begin
    widget_control, wd, get_value = idraw
    idraw.select
    idraw.erase
  endif
  
  ran1_x = xx[ind_min_I - x_ran]/10
  ran2_x = xx[ind_min_I + x_ran]/10
  
  item_ID = data_ID
  if wd gt 0 then begin
    ; Obtain the window index
    widget_control, wd, get_value = idraw
    ; Set the new widget to be the current graphics window
    idraw.select
    ; No overplotting
    idraw.erase
    ; Plot data
    p = plot(wavelength, intensity, xtit = '$\lambda$ [nm]' ,ytit='!8I!3 !7[a. u.]!3', symbol = 3, sym_size = 2, sym_fill_color = 'maroon', /current $
      , xrange = [wavelength[0], wavelength[-1]], xstyle = 1, yrange=[minimum - 0.01 , maximum + 0.01], color = 'maroon' $
      , title = shortname[item_ID], font_size=16, BACKGROUND_COLOR = [246,222,190], xthick = 2, ythick = 2, xticklen = 0.02 $
      , yticklen = 0.02, FONT_NAME = 'Times', NAME = 'Spectral profile')
    y2 = axis('y', location = 'right', tickvalues = [I0, IC], tickname = ['!8 I!3!D!70!3', ' !8 I!3!D!7C!3'],  tickfont_size=16, tickfont_name = 'Times' $
    , ticklen = 0.0)
    line_b = plot([ran1_x, ran2_x], [I0, I0], thick = 3, color = 'green', /overplot)
    line_t = plot([ran1_x, ran2_x], [IC, IC], thick = 3, color = 'green', /overplot)
    ; add v to the x-axis
  endif

end

;-------------------------------------------------------------------------------------------------------------------------------------------------
; Plot bisector procedure
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro Bisector, Event, item_ID
  common CB_data, wavelength, intensity
  common CB_Imin, I_min
  common CB_ind_min_I, ind_min_I
  common CB_I, I
  common CB_bise_x, bise_x
  common CB_bise_y, bise_y

  ; Halfs of the profile
  wavelength_1 = wavelength[0:I_min]
  wavelength_2 = wavelength[I_min:*]
  intensity_1   = intensity[0:I_min]
  intensity_2   = intensity[I_min:*]
  
  ; Find values in intensity_1 and intensity_2 that are closest to the intensities in I
  best_I1 = LIST()
  best_I2 = LIST()
  best_w1 = LIST()
  best_w2 = LIST()

  for j = 0, N_ELEMENTS(I) - 1 do begin
    tem1 = min(abs(intensity_1 - I[j]), ind1)
    tem2 = min(abs(intensity_2 - I[j]), ind2)
    best_I1.Add, intensity_1[ind1]
    best_I2.Add, intensity_2[ind2]
    ; Find corresponding wavelength
    best_w1.Add, wavelength_1[ind1]
    best_w2.Add, wavelength_2[ind2]
  endfor
 
  bisector_x = LIST()
  bisector_y = LIST()
  
  ; Compute bisectors
  for k=0, N_ELEMENTS(best_I1)-1 do begin
    bis_x = (best_w1[k] + best_w2[k])/2
    bisector_x.Add, bis_x
    bis_y = (best_I1[k] + best_I2[k])/2
    bisector_y.Add, bis_y
  endfor

  bise_x = bisector_x.toArray()
  bise_y = bisector_y.toArray()

end

;-------------------------------------------------------------------------------------------------------------------------------------------------
; Plot bisector procedure
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro PlotBisector, Event
  common CB_p, p
  common CB_bise_x, bise_x
  common CB_bise_y, bise_y
  common CB_ev, wWidget
  
  ; Plot bisectors
  wd = widget_info(wWidget, find_by_uname='WID_DRAW_0')
  if wd gt 0 then begin
    ; Obtain the window index
    widget_control, wd, get_value = idraw
    ; Set the new widget to be the current graphics window
    idraw.select
    ; Plot
    bisec = plot(bise_x, bise_y, /overplot, symbol = 3, sym_size = 2, NAME = 'Bisector')
    leg = LEGEND(TARGET = [p, bisec], POSITION = [0.9, 0.5], /NORMAL, /AUTO_TEXT_COLOR, THICK = 2, TRANSPARENCY = 80, FONT_NAME = 'Times'$
      ,font_size = 13)
  endif
  
end

;-------------------------------------------------------------------------------------------------------------------------------------------------
; Computing bisector velocity span procedure - verify the inserted values
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro ComputeBVS_1, Event
  common CB_ran, ran1_x, ran2_x
  common CB_intensity, IC, I0, I1, I2
  common CB_data, wavelength, intensity
  common CB_ev, wWidget
  common CB_tab, lam_0, slider1, slider2, w
  common CB_Dop, v, c
  common CB_xx, xx
  common CB_ind_min_I, ind_min_I
  common CB_x_ran, x_ran
  common CB_ran_I, ran_I

  ; Erase error window
  wd_er = widget_info(wWidget, find_by_uname='WID_DRAW_ERR')
  if wd_er gt 0 then begin
    widget_control, wd_er, get_value = idraw
    ; Set the new widget to be the current graphics window
    idraw.select
    idraw.erase
  endif
  
; Save values set by user
  slid_I1 = widget_info(wWidget, find_by_uname = 'SLIDER1')
  if slid_I1 gt 0 then begin
    widget_control, slid_I1, get_value = slider1
  endif
  
  slid_I2 = widget_info(wWidget, find_by_uname = 'SLIDER2')
  if slid_I2 gt 0 then begin
    widget_control, slid_I2, get_value = slider2
  endif
  
  width = widget_info(wWidget, find_by_uname='STEP')
  if width gt 0 then begin
    widget_control, width, get_value=wi
  endif
  
  w = FLOAT(wi[0]*0.01*ran_I)
  lambda_0 = widget_info(wWidget, find_by_uname='lambda_0')
  if lambda_0 gt 0 then begin
    widget_control, lambda_0, get_value=lamb_0
  endif
  lam_0 = FLOAT(lamb_0[0])

  ; Checking inserted values
  ; width  
  if w eq 0 then begin
    text_w = 'Error: choose a width of belts.'
    wd = widget_info(wWidget, find_by_uname='WID_DRAW_ERR')
    if wd gt 0 then begin
      widget_control, wd, get_value = idraw
      ; Set the new widget to be the current graphics window
      idraw.select
      t = TEXT(0.01, 0.73, text_w, 'r', /RELATIVE, FONT_NAME = fon $
        , FONT_SIZE = 13, FONT_STYLE = "bf", HIDE = 0)
    endif
  endif else begin
    if ((4 * w) lt (I2 - I1)) and ((I0+slider1*0.01*ran_I - w) gt I1) and ((1-slider2*0.01*ran_I + w) lt I2) and ((1-slider2*0.01*ran_I - I0+slider1*0.01*ran_I) gt (2 * w)) and (w gt 0) then begin
      width = w
      wd = widget_info(wWidget, find_by_uname='WID_DRAW_ERR')
      if wd gt 0 then begin
        widget_control, wd, get_value = idraw
        ; Set the new widget to be the current graphics window
        idraw.select
        idraw.erase
      endif
    endif else begin
      text_w = 'Error: the belts are of wrong width. They have to include the bisector line and do not overlap each other.'
      wd = widget_info(wWidget, find_by_uname='WID_DRAW_ERR')
      if wd gt 0 then begin
        widget_control, wd, get_value = idraw
        ; Set the new widget to be the current graphics window
        idraw.select
        t = TEXT(0.01, 0.73, text_w, 'r', /RELATIVE, FONT_NAME = fon $
          , FONT_SIZE = 13, FONT_STYLE = "bf")
      endif
    endelse
  endelse
  
  ; lambda_0
  if lam_0 le 0 then begin
    text_l = 'Error: choose a positive central wavelength.'
    wd = widget_info(wWidget, find_by_uname='WID_DRAW_ERR')
    if wd gt 0 then begin
      widget_control, wd, get_value = idraw
      ; Set the new widget to be the current graphics window
      idraw.select
      t2 = TEXT(0.01, 0.55, text_l, 'r', /RELATIVE, FONT_NAME = fon $
        , FONT_SIZE = 13, FONT_STYLE = "bf")
    endif
  endif
  
  if lam_0 gt 0 then begin
    ; Convert wavelength to velocity
    c = 299792.458                    ; km/s
    v = c * (wavelength*10^(-12) - lam_0*10^(-12)) / (lam_0*10^(-12))
    ran1_x = xx[ind_min_I - x_ran]/10.
    ran2_x = xx[ind_min_I + x_ran]/10.  
  endif
  
end

;-------------------------------------------------------------------------------------------------------------------------------------------------
; Determining areas for computing BVS
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro ComputeBVS_2, Event
  common CB_ran, ran1_x, ran2_x
  common CB_tab, lam_0, slider1, slider2, w
  common CB_x, x
  common CB_bounds, up_y1, up_y2, down_y1, down_y2, down1, down2, up1, up2
  common CB_intensity, IC, I0, I1, I2
  common CB_ran_I, ran_I
  
  ; Determine top and bottom area to compute BVS
  Int_1 = I0+slider1*0.01*ran_I
  Int_2 = 1-slider2*0.01*ran_I
  width = w
  up1 = [Int_1 + width]
  down1 = [Int_1 - width]
  x = [ran1_x : ran2_x : (ran2_x - ran1_x)/100]
  up_y1 = rebin(up1, N_ELEMENTS(x))
  down_y1 = rebin(down1, N_ELEMENTS(x))
  up2 = [Int_2 + width]
  down2 = [Int_2 - width]
  up_y2 = rebin(up2, N_ELEMENTS(x))
  down_y2 = rebin(down2, N_ELEMENTS(x))

end 

;-------------------------------------------------------------------------------------------------------------------------------------------------
; Calculation of BVSI
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro ComputeBVS_3I, Event, i
  common CB_bise_x, bise_x
  common CB_bise_y, bise_y
  common CB_Result, Result
  common CB_bounds, up_y1, up_y2, down_y1, down_y2, down1, down2, up1, up2
  common CB_V, VB_x, VT_x, VB_y, VT_y
  common CB_Result, Result
  common CB_BVS, BVS
  common CB_tab, lam_0, slider1, slider2, w
  common CB_V, VB_x_l, VT_x_l, VB_y_l, VT_y_l



  ; Compute the average values of velocities in the top and bottom zomes: V_T and V_B
  ind1 = where((bise_y ge down1[0]) and (bise_y le up1[0]))
  ind2 = where((bise_y ge down2[0]) and (bise_y le up2[0]))

  ; Computing average values of velocities in the top and bottom zones
  c = 299792.458                    ; km/s  
  
  VB_x = c*(bise_x[ind1]-lam_0)/lam_0
  VT_x = c*(bise_x[ind2]-lam_0)/lam_0
  VB_y = c*(bise_y[ind1]-lam_0)/lam_0
  VT_y = c*(bise_y[ind2]-lam_0)/lam_0 

  VB = mean(VB_x)
  VT = mean(VT_x)

  ; Computing BVS (bisector velocity span)
  BVS    = VT - VB
  st_BVS = STRING(BVS)
  Result = STRMID(st_BVS, 0, 14)
  
  VB_x_l = bise_x[ind1]
  VT_x_l = bise_x[ind2]
  VB_y_l = bise_y[ind1]
  VT_y_l = bise_y[ind2]

end

;-------------------------------------------------------------------------------------------------------------------------------------------------
; Calculation of BVS
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro ComputeBVS_3, Event, i
  common CB_bise_x, bise_x
  common CB_bise_y, bise_y
  common CB_Result, Result
  common CB_bounds, up_y1, up_y2, down_y1, down_y2, down1, down2, up1, up2
  common CB_V, VB_x, VT_x, VB_y, VT_y
  common CB_Result, Result
  common CB_BVS, BVS
  common CB_set, lam_02, slider12, slider22, w2


  ; Compute the average values of velocities in the top and bottom zomes: V_T and V_B

  ind1 = where((bise_y ge down1[i]) and (bise_y le up1[i]))
  ind2 = where((bise_y ge down2[i]) and (bise_y le up2[i]))   
    
  ; Computing average values of velocities in the top and bottom zones
  c = 299792.458                    ; km/s    
    
    
  VB_x = c*(bise_x[ind1]-lam_02)/lam_02
  VT_x = c*(bise_x[ind2]-lam_02)/lam_02
  VB_y = c*(bise_y[ind1]-lam_02)/lam_02
  VT_y = c*(bise_y[ind2]-lam_02)/lam_02 
 

  
  VB = mean(VB_x)
  VT = mean(VT_x)

  ; Computing BVS (bisector velocity span) 
  BVS    = VT - VB
  st_BVS = STRING(BVS)
  Result = STRMID(st_BVS, 0, 14)    

end

;-------------------------------------------------------------------------------------------------------------------------------------------------
; Plotting graphic to determining BVS
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro DisplayBVS, Event
  common CB_bise_x, bise_x
  common CB_bise_y, bise_y
  common CB_p, p
  common CB_ev, wWidget
  common CB_tab, lam_0, slider1, slider2, w
  common CB_Result, Result
  common CB_x, x
  common CB_bounds, up_y1, up_y2, down_y1, down_y2
  common CB_V, VB_x, VT_x, VB_y, VT_y
  common CB_Dop, v, c
  common CB_tab, lam_0, slider1, slider2, w
  common CB_V, VB_x_l, VT_x_l, VB_y_l, VT_y_l

  ; Display BVS in widget
  widget_control, widget_info(wWidget, find_by_uname='BVS'), set_value = 'BVS =' + Result
  
  wd = widget_info(wWidget, find_by_uname='WID_DRAW_0')
  if (wd gt 0) and (lam_0 gt 0) and (w gt 0) then begin
    ; Obtain the window index
    widget_control, wd, get_value = idraw
    ; Set the new widget to be the current graphics window
    idraw.select
    idraw.erase
    ; Drawing procedures
    PlottingData
    Bisector


    ; Plot bottom and top regions
    poly1 = POLYGON([x, reverse(x)], [up_y1, reverse(down_y1)], /DATA, /FILL_BACKGROUND, FILL_COLOR = "light blue", TRANSPARENCY = 70 $
      , linestyle = 6)    
    poly2 = POLYGON([x, reverse(x)], [up_y2, reverse(down_y2)], /DATA, /FILL_BACKGROUND, FILL_COLOR = "light blue", TRANSPARENCY = 70 $
      , linestyle = 6)
    bisec = plot(bise_x, bise_y, /overplot, symbol = 3, sym_size = 2, NAME = 'Bisector')
    leg = LEGEND(TARGET = [p, bisec], POSITION = [0.9, 0.5], /NORMAL, /AUTO_TEXT_COLOR, THICK = 2, TRANSPARENCY = 80, FONT_NAME = 'Times'$
      ,font_size = 13)
    VBx = plot(VB_x_l, VB_y_l, color = 'purple', /overplot, symbol = 3, sym_size = 2)
    VTy = plot(VT_x_l, VT_y_l, color = 'purple', /overplot, symbol = 3, sym_size = 2)
    ax = p.axes
    ax[0].hide=1
    axis_v = axis('x', location = 'bottom', axis_range = [v[0], v[-1]], tit = '!8v!3 !7[km/s]!3', tickfont_size=16, tickfont_name = 'Times' $
      , thick = 2, ticklen = 0.02, coord_transform = [-lam_0, 1] * c/lam_0)
  endif
   
end

;-------------------------------------------------------------------------------------------------------------------------------------------------
; Computing ranges of intensities for bisectors of all files
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro ComI1I2
  common CB_I0_All, I0_All
  common CB_values, coef1, coef2
  common CB_step, step
  common CB_all_intensities, I1_all, I2_all, I_all, BH_all
  common CB_ran_I_all, ran_I_all
  common CB_I02, I02

  I02 = I0_All.toArray()
  IC = 1
  ran_I_all = IC - I02

  ; Create empty lists for I1, I2, I_all for all data files
  I1_all_l = LIST()
  I2_all_l = LIST()
  I_all_l  = LIST()
  BH_all_l = LIST()

  ; Compute I1, I2, I_all for all files
  for i = 0, N_ELEMENTS(I02) - 1 do begin
    I1_a = I02[i] + FLOAT(coef1*0.01) * ran_I_all[i]  ; lower bound of the bisector line
    I2_a = IC - FLOAT(coef2*0.01) * ran_I_all[i]   ; upper bound of the bisector line
    I1_all_l.Add, I1_a
    I2_all_l.Add, I2_a
    I_a = [I1_a : I2_a : FLOAT(step)]
    I_all_l.Add, I_a
    BH_a = I1_a + (I2_a - I1_a)/2           ; center of intensity of a bisector
    BH_all_l.Add, BH_a
  endfor

  I1_all = I1_all_l.toArray()
  I2_all = I2_all_l.toArray()
  BH_all = BH_all_l.toArray()

end

;-------------------------------------------------------------------------------------------------------------------------------------------------
; Preparing values for determining all BVSs
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro ComputeBVS_2_all, Event
  common CB_ran, ran1_x, ran2_x
  common CB_data, wavelength, intensity
  common CB_set, lam_02, slider12, slider22, w2
  common CB_x, x
  common CB_bounds, up_y1, up_y2, down_y1, down_y2, down1, down2, up1, up2
  common CB_Dop, v, c
  common CB_xx, xx
  common CB_ind_min_I, ind_min_I
  common CB_x_ran, x_ran
  common CB_ran_I, ran_I
  common CB_ran_I_all, ran_I_all

  ; Convert wavelength to velocity
  c = 299792.458                    ; km/s
  
  v = c * (wavelength*10^(-12) - lam_02*10^(-12)) / (lam_02*10^(-12))

  ran1_x = xx[ind_min_I - x_ran]/10.
  ran2_x = xx[ind_min_I + x_ran]/10.
  ; Determine top and bottom area to compute BVS
  
  
  up1 = LIST()
  up2 = LIST()
  down1 = LIST()
  down2 = LIST()
  
  for i = 0, N_ELEMENTS(slider12) - 1 do begin
  
    Int_1 = slider12[i]
    Int_2 = slider22[i]
    width = w2[i]
    up1_i = Int_1 + width

    down1_i = Int_1 - width
    x = [ran1_x : ran2_x : (ran2_x - ran1_x)/100]
    up2_i = Int_2 + width
    down2_i = Int_2 - width

    up1.Add, up1_i
    up2.Add, up2_i
    down1.Add, down1_i
    down2. Add, down2_i
    
    ;up_y1 = rebin(up1, N_ELEMENTS(x))
 
    ;down_y1 = rebin(down1, N_ELEMENTS(x))
    ;up_y2 = rebin(up2, N_ELEMENTS(x))
    ;down_y2 = rebin(down2, N_ELEMENTS(x))

  endfor
  
end

;-------------------------------------------------------------------------------------------------------------------------------------------------
; Computing BVS for all loaded data files - window with properties
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro AllBVS, Event
  common CB_size_sc, size_sc
  common CB_AllBase, AllBase
  common CB_step, step
  common CB_all_intensities, I1_all, I2_all, I_all, BH_all
    
  fon = 'TIMES'

  sl_width = 0.2 * size_sc[0]
  sl_high  = 0.1 * size_sc[1]
  
  m = String(13B)
  
  AllBase = WIDGET_BASE(UNAME='AllBase', XOFFSET = 0.21 * size_sc[0], YOFFSET = 0.2 * size_sc[1] $
    , SCR_XSIZE = 0.63 * size_sc[1], SCR_YSIZE = 0.6 * size_sc[1], TITLE='Compute BVS for all spectral line profiles - settings')
    
  OK_All = Widget_Button(AllBase, UNAME='OK_All', VALUE = 'OK', FONT = fon $
    , XOFFSET= 0.26 * size_sc[0], YOFFSET = 0.3 * size_sc[1], XSIZE = 0.075 * size_sc[0], YSIZE = 1.4 * sl_high/2)
  
  Label12 = Widget_label(AllBase, value = 'Choose intensities of belts for computing bisector velocity span (BVS):' $
    , ysize = 0.026 * size_sc[1], XOFFSET = 0.01 * size_sc[0], YOFFSET = 0.0052 * size_sc[1], FONT = fon)

  Buttom_CW_FSL2 = Widget_Base(AllBase, UNAME='Buttom_CW_FSL2'  $
    , XOFFSET= 0.01 * size_sc[0], YOFFSET = 0.05 * size_sc[1] , SCR_XSIZE = 0.34 * size_sc[0], SCR_YSIZE = sl_high + 20)

  SLIDER12 = CW_FSLIDER(Buttom_CW_FSL2, UNAME='SLIDER12', TITLE='Intensity of bottom part (% from I0)', /EDIT, FORMAT = '(G10.3)' $
    , XSIZE = 0.32 * size_sc[0], YSIZE = sl_high, FRAME = 1, MAXIMUM = 50, MINIMUM = 0)

  Buttom_CW_FSL_22 = Widget_Base(AllBase, UNAME='Buttom_CW_FSL22'  $
    , XOFFSET = 0.01 * size_sc[0], YOFFSET = 0.05 * size_sc[1] + sl_high + 40 + 0.026 * size_sc[1], SCR_XSIZE = 0.34 * size_sc[0] ,SCR_YSIZE=sl_high+40)

  SLIDER22 = CW_FSLIDER(Buttom_CW_FSL_22, UNAME='SLIDER22', TITLE='Intensity of top part (% from IC)', /EDIT, FORMAT = '(G10.3)' $
    , XSIZE = 0.32 * size_sc[0], YSIZE=sl_high, FRAME = 1, MAXIMUM = 50, MINIMUM = 0)

  Label32 = Widget_label(AllBase, value = 'Width of belts (%):', ysize = 0.026 * size_sc[1], XOFFSET = 0.01 * size_sc[0] $
    , YOFFSET = 0.3 * size_sc[1], FONT = fon)

  STEP2 = Widget_Text(AllBase, UNAME='STEP2', /EDITABLE, XOFFSET = 0.01 * size_sc[0], YOFFSET = 0.33 * size_sc[1] $
    , XSIZE=18 ,YSIZE=1, FONT = fon, VALUE = '0')

  Label42 = Widget_label(AllBase, value = 'Central wavelength (nm):', ysize = 0.026 * size_sc[1], XOFFSET = 0.133 * size_sc[0] $
    , YOFFSET = 0.3 * size_sc[1], FONT = fon)

  lambda_02 = Widget_Text(AllBase, UNAME='lambda_02', /EDITABLE, XOFFSET = 0.133 * size_sc[0], YOFFSET = 0.33 * size_sc[1] $
    , XSIZE=18 ,YSIZE=1, FONT = fon, VALUE = '0')
    
  ; Plot widget if there is an error
  WID_DRAW_ERR2 = Widget_Window(AllBase, UNAME = 'WID_DRAW_ERR2', XOFFSET = 0.01 * size_sc[0] $
    , YOFFSET = 0.38 * size_sc[1], XSIZE = 0.325 * size_sc[0], YSIZE = 0.15 * size_sc[1])
      
  WIDGET_CONTROL, AllBase, /REALIZE
  XManager, 'AllBVS', AllBase, event_handler='AllBVS_ev'

end

;-------------------------------------------------------------------------------------------------------------------------------------------------
; Event for governing OK button
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro AllBVS_ev, Event

  if Widget_Info(Event.id, FIND_BY_UNAME='OK_All') eq Event.id then begin
    if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      OKAll, Event
  end

end

;-------------------------------------------------------------------------------------------------------------------------------------------------
; OK button used for computing BVS for all files
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro OKAll, Event
  common CB_AllBase, AllBase
  common CB_set, lam_02, slider12, slider22, w2
  common CB_shortname, shortname
  common CB_error_files, error_files
  common CB_all_intensities, I1_all, I2_all, I_all, BH_all
  common CB_ran_I_all, ran_I_all
  common CB_I02, I02

  ; Clear all errors
  wd2 = widget_info(event.top, find_by_uname='WID_DRAW_ERR2')
  if wd2 gt 0 then begin
    widget_control, wd2, get_value = idraw
    idraw.select
    idraw.erase
  endif

  ; Save values set by user
  slid_I1_All = widget_info(event.top, find_by_uname = 'SLIDER12')
  if slid_I1_All gt 0 then begin
    widget_control, slid_I1_All, get_value = sli12
  endif

  slider12 = I02+sli12*0.01*ran_I_all
  
  slid_I2_All = widget_info(event.top, find_by_uname = 'SLIDER22')
  if slid_I2_All gt 0 then begin
    widget_control, slid_I2_All, get_value = sli22
  endif

  slider22 = 1-sli22*0.01*ran_I_all
  
  width_all = widget_info(event.top, find_by_uname='STEP2')
  if width_all gt 0 then begin
    widget_control, width_all, get_value=wi2
  endif
  w2 = FLOAT(wi2[0])*0.01*ran_I_all
  
  lambda_02 = widget_info(event.top, find_by_uname='lambda_02')
  if lambda_02 gt 0 then begin
    widget_control, lambda_02, get_value=lamb_02
  endif

  lam_02 = FLOAT(lamb_02[0])
  

  ; Error for empty width and lambda_0
  ind_zero = where(w2 eq 0, count)

  if count gt 0 then begin
    text_w2 = 'Error: choose a width of belts.'
    wd2 = widget_info(event.top, find_by_uname='WID_DRAW_ERR2')
    if wd2 gt 0 then begin
      widget_control, wd2, get_value = idraw
      idraw.select
      t2 = TEXT(0.01, 0.8, text_w2, 'r', /RELATIVE, FONT_NAME = fon $
        , FONT_SIZE = 13, FONT_STYLE = "bf", HIDE = 0)
    endif
  endif

  if lam_02 eq 0 then begin
    text_l2 = 'Error: choose a positive central wavelength.'
    wd2 = widget_info(event.top, find_by_uname='WID_DRAW_ERR2') 
    if wd2 gt 0 then begin
      widget_control, wd2, get_value = idraw
      idraw.select
      t2 = TEXT(0.01, 0.6, text_l2, 'r', /RELATIVE, FONT_NAME = fon $
        , FONT_SIZE = 13, FONT_STYLE = "bf", HIDE = 0)
    endif
  endif
  


  ; Checking set width - save number of file that is wrong
  error_item = LIST()
  if (count eq 0) and (lam_02 ne 0) then begin
    ; Find files for which the set with is inappropriate
    for i = 0, N_ELEMENTS(I1_all) - 1 do begin
      if ((4 * w2[i]) lt (I2_all[i] - I1_all[i])) and ((slider12[i] - w2[i]) gt I1_all[i]) and ((slider22[i] + w2[i]) lt I2_all[i]) and ((slider22[i] - slider12[i]) gt (2 * w2[i])) and (w2[i] gt 0) then begin
        continue
      endif else begin
        error_it = i
        error_item.Add, error_it
      endelse
    endfor

    
    error_files = shortname[error_item.toArray()]

    ; Displaying error about wrong width - example of files that are not appropiate for the given settings are displayed
    if N_ELEMENTS(error_item) ne 0 then begin
      if N_ELEMENTS(error_item) ge 4 then begin
        n_file = 4
      endif else begin
        n_file = N_ELEMENTS(error_item)
      endelse
      text_w_error = 'Error - wrong width in files, for instance:'
      wd2_f = widget_info(event.top, find_by_uname='WID_DRAW_ERR2')
      if wd2_f gt 0 then begin
        widget_control, wd2_f, get_value = idraw
        idraw.select
        t_error = TEXT(0.01, 0.76, text_w_error, 'r', /RELATIVE, FONT_NAME = fon $
          , FONT_SIZE = 13, FONT_STYLE = "bf")
        t_files = TEXT(0.01, 0.06, error_files[0:n_file-1], 'r', /RELATIVE, FONT_NAME = fon $
          , FONT_SIZE = 13)
      endif
    endif
  endif

  ; Close the window if width, lambda0 are correct
  if (count eq 0) and (lam_02 gt 0) and (N_ELEMENTS(error_files) eq 0) then begin
    WIDGET_CONTROL, AllBase, /destroy
  endif

end

;-------------------------------------------------------------------------------------------------------------------------------------------------
; Computing BVS for all loaded data files
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro ComputeAllBVS, Event
  common CB_shortname, shortname
  common CB_I0_All, I0_all
  common CB_bise_lx, bise_lx
  common CB_bise_ly, bise_ly
  common CB_Result_all, Result_all
  common CB_ev, wWidget

  I0_all     = LIST()
  bise_lx    = LIST()
  bise_ly    = LIST()
  Result_all = LIST()
  
  ; Obtain list of I0 for all files
  for i = 0, N_ELEMENTS(shortname) - 1 do begin
    OpenFile, Event, i
    SaveI0
  endfor
    
  COMI1I2
  AllBVS

  for i = 0, N_ELEMENTS(shortname) - 1 do begin
    OpenFile, Event, i
    SaveI0 
    Bisector
    ComputeBVS_2_all
    ComputeBVS_3, Event, i
    SaveBVS
  endfor
  
  ; Information that BVS were computed and saved to a file
  wd = widget_info(wWidget, find_by_uname='WID_DRAW_ERR')
  if wd gt 0 then begin
    widget_control, wd, get_value = idraw
    idraw.select
    idraw.erase
    RV_loaded = TEXT(0.01, 0.7, 'BVS for all data files have been computed and saved to file BVS.txt.', 'navy', /RELATIVE, FONT_NAME = fon $
      , FONT_SIZE = 13, FONT_STYLE = "bf", HIDE = 0)
    ; The printed text will be erased in 1.5 seconds
    WAIT, 1.5
    idraw.erase
  endif
  
end

;-------------------------------------------------------------------------------------------------------------------------------------------------
; Saving BVSs of all files to a list and a file
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro SaveBVS
  common CB_Result, Result
  common CB_Result_all, Result_all
  common CB_shortname, shortname
  
  Result_all.Add, FLOAT(Result)

  fname = 'BVS.txt'
  OPENW, 1, fname
  for i = 0, N_ELEMENTS(Result_all) - 1 do PRINTF, 1, shortname[i], Result_all[i]
  CLOSE, 1

end

;-------------------------------------------------------------------------------------------------------------------------------------------------
; Displaying table of results procedure
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro TableOfResults, Event
  common CB_ev, wWidget
  common CB_Result_all, Result_all
  common CB_RV, RV
  common CB_Number_file, Number_file
  common CB_shortname, shortname

  ; Number of elements in the list
  ERe = N_ELEMENTS(Result_all)
  ERV = N_ELEMENTS(RV)

  ; Error - lists of RVs or BVSs are empty
  wd = widget_info(wWidget, find_by_uname='WID_DRAW_ERR')
  if (ERe eq 0) or (ERV eq 0) then begin
    if wd gt 0 then begin
      widget_control, wd, get_value = idraw
      idraw.select
      idraw.erase
      BVS_error = TEXT(0.01, 0.7, 'Error: all BVS have not been computed or a file with another RV has not been loaded.', 'red', /RELATIVE, FONT_NAME = fon $
        , FONT_SIZE = 13, FONT_STYLE = "bf")
    endif
  endif else begin
    ; Error - number of RVs and BVSs is different
    if ERe ne ERV then begin
      if wd gt 0 then begin
        widget_control, wd, get_value = idraw
        idraw.select
        idraw.erase
        BVS_error = TEXT(0.01, 0.7, 'Error: the number of computed BVS is different from the number of radial velocities.', 'red', /RELATIVE, FONT_NAME = fon $
          , FONT_SIZE = 13, FONT_STYLE = "bf", HIDE = 0)
      endif
    endif else begin
      ; All is fine - erase error panel
      if wd gt 0 then begin
        widget_control, wd, get_value = idraw
        idraw.select
        idraw.erase
      endif
      ; Display table and fill it with BVSs and RVs
      widget_control, widget_info(wWidget, find_by_uname='WID_Tab'), MAP = 1
      widget_control, widget_info(wWidget, find_by_uname='Table'), set_value = [TRANSPOSE(Result_all.toArray()), TRANSPOSE(RV)]
    endelse
  endelse

  match = LIST()

  ; Checking if radial velocities correspont to BVS
  for i = 0, N_ELEMENTS(Number_file) - 1 do begin
    ; Look for numbers in RV file in names of data files
    match_i = STRMATCH(shortname[i], '*' + Number_file[i] + '*')
    ; Add 0 and 1 to list - 1 is true
    match.Add, match_i
  endfor

  ; Couont errors - 0
  ind_err = where(0 eq match, count)

  ; Error that numbers of files in RV does not correspond to names of files
  if count ne 0 then begin
    wd = widget_info(wWidget, find_by_uname='WID_DRAW_ERR')
    if wd gt 0 then begin
      text_match = 'The file with RVs seems to not correspond to loaded data according to names.'
      widget_control, wd, get_value = idraw
      idraw.select
      t_err_match = TEXT(0.01, 0.5, text_match, 'r', /RELATIVE, FONT_NAME = fon $
        , FONT_SIZE = 13, FONT_STYLE = "bf", HIDE = 0)
      WAIT, 2
      idraw.erase
    endif
  endif

end

;-------------------------------------------------------------------------------------------------------------------------------------------------
; Correlation procedure
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro Correlation_RV_BVS, Event
  common CB_Result_all, Result_all
  common CB_RV, RV
  common CB_ev, wWidget
  common CB_Number_file, Number_file
  common CB_shortname, shortname
  
  ; Hide the table of resulting values
  widget_control, widget_info(wWidget, find_by_uname='WID_Tab'), MAP = 0
  
  ERe = N_ELEMENTS(Result_all)
  ERV = N_ELEMENTS(RV)
  
  ; Error when BVSs or RVs are not computed
  wd = widget_info(wWidget, find_by_uname='WID_DRAW_ERR')
  if (ERe eq 0) or (ERV eq 0) then begin
    if wd gt 0 then begin
      widget_control, wd, get_value = idraw
      idraw.select
      idraw.erase
      BVS_error = TEXT(0.01, 0.7, 'Error: all BVS have not been computed or a file with RVs has not been loaded.', 'red', /RELATIVE, FONT_NAME = fon $
        , FONT_SIZE = 13, FONT_STYLE = "bf")
    endif 
  endif else begin
    ; Error when the number of elements of RVs does not correspond to that in BVSs
    if ERe ne ERV then begin
      if wd gt 0 then begin
        widget_control, wd, get_value = idraw
        idraw.select
        idraw.erase
        BVS_error = TEXT(0.01, 0.7, 'Error: the number of computed BVS is different from the number of RVs.', 'red', /RELATIVE, FONT_NAME = fon $
          , FONT_SIZE = 13, FONT_STYLE = "bf")
        ; The printed text will be erased in 2 seconds
      endif      
    endif else begin
      ; Compute correlation 
      if wd gt 0 then begin
        widget_control, wd, get_value = idraw
        idraw.select
        idraw.erase
      endif
      corr_res = CORRELATE(Result_all.toArray(), RV)
      text_corr = corr_res.toString()
      wd = widget_info(wWidget, find_by_uname='WID_DRAW_0')
      
      ; Display correlation of RVs and BVSs
      if wd gt 0 then begin
        widget_control, wd, get_value = idraw
        idraw.select
        idraw.erase
        x1 = min(Result_all.toArray())
        x2 = max(Result_all.toArray())
        r1 = x1 - 0.1 * (x2-x1)
        r2 = x2 + 0.1 * (x2-x1)
        result_c = corr_res.toString()
        p = SCATTERPLOT(Result_all.toArray(), RV, xtit = 'BVS [km/s]', ytit='RV [km/s]', sym_color = 'maroon', /SYM_FILLED, /current $
          , xstyle = 1, xrange = [r1, r2] $
          , title = 'Correlation', font_size=16, BACKGROUND_COLOR = [246,222,190], xthick = 2, ythick = 2, xticklen = 0.02 $
          , yticklen = 0.02, FONT_NAME = 'Times')
      endif
      
      ; Display information about correlation
      wd = widget_info(wWidget, find_by_uname='WID_DRAW_ERR')
      if wd gt 0 then begin
        widget_control, wd, get_value = idraw
        idraw.select
        idraw.erase
        result = TEXT(0.01, 0.7, 'Correlation of BVS and RV: ' + text_corr, /RELATIVE, FONT_NAME = fon $
          , FONT_SIZE = 13, FONT_STYLE = "bf")
      endif
    endelse
  endelse
  
  ; Checking wheter names of files in RV file correspond to loaded files
  match = LIST()
  ; Checking if radial velocities correspont to BVS
  for i=0, N_ELEMENTS(Number_file)-1 do begin
    match_i = STRMATCH(shortname[i], '*' + Number_file[i] + '*')
    match.Add, match_i
  endfor
  
  ind_err = where(0 eq match, count)

  if count ne 0 then begin
    wd = widget_info(wWidget, find_by_uname='WID_DRAW_ERR')
    if wd gt 0 then begin
      text_match = 'The file seems to not correspond to loaded data according to names.'
      widget_control, wd, get_value = idraw
      ; Set the new widget to be the current graphics window
      idraw.select
      t_err_match = TEXT(0.01, 0.5, text_match, 'r', /RELATIVE, FONT_NAME = fon $
        , FONT_SIZE = 13, FONT_STYLE = "bf", HIDE = 0)
      WAIT, 2
      idraw.erase
    endif
  endif
  
end