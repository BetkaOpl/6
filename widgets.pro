;-------------------------------------------------------------------------------------------------------------------------------------------------
; IDL Widget Interface procedure 
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro WID_BASE_0_event, Event ; structure
  common CB_shortname, shortname
  common CB_ev, wWidget

  ; Find link of graphical object where the Event arrise
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)

  ; Calling procedures
  wWidget = Event.top
  
  ; Cases for the Event
  case wTarget of

    Widget_Info(wWidget, FIND_BY_UNAME='WID_BASE_0'): begin
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_1'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON')then $
        LoadMyFiles, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='RV_1'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON')then $
        LoadRV, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_LIST_0',/list_select): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_LIST' )then $
        if event.clicks eq 2 then begin
        OpenFile, Event, event.index
        OpenErrors, Event, event.index
        PlottingData
        endif
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_2'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON')then $
        AddFile, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_3'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON')then $
        Settings
        BisectorI        ; after settings the new graph with bisector is displayed
        PlottingData
        Bisector
        PlotBisector
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_4'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON')then $
        ExitMyApp, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='Corr_0'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON')then $
        TableOfResults
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_9'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON')then $
        HelpWindow
    end
    Widget_Info(wWidget, FIND_BY_UNAME='Calculate_bisec'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON')then $
        Bisector
        PlotBisector
    end 
    Widget_Info(wWidget, FIND_BY_UNAME='Calculate_BVS'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON')then $
        ComputeBVS_1
        ComputeBVS_2
        Bisector
        ComputeBVS_3I
        DisplayBVS
    end
    Widget_Info(wWidget, FIND_BY_UNAME='AllBVS'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON')then $
        ComputeAllBVS, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='Corr_1'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON')then $
        Correlation_RV_BVS
    end
    Widget_Info(wWidget, FIND_BY_UNAME='Remove', /list_select): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON')then $
        highlighted_item = Widget_Info(Widget_Info(wWidget, FIND_BY_UNAME='W_LIST_0'), /list_select)
        RemoveFile, Event, highlighted_item
        WIDGET_CONTROL, Widget_Info(wWidget, FIND_BY_UNAME='W_LIST_0'), set_value = shortname
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_4'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON')then $
        widget_control, Event.top, /destroy
    end
    else:
  endcase

end

;-------------------------------------------------------------------------------------------------------------------------------------------------
; IDL Widget procedure 
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro WID_BASE_0, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
  common CB_size_sc, size_sc
  common WID_CB, WID_BASE_0
  common CB_id_list_0, id_list_0 ; idenfikátor draw, list
  common CB_shortname_add, shortname_add
  common CB_ev, wWidget
  common CB_shortname, shortname

  ; Load event callback routines
  Resolve_Routine, 'procedures',/COMPILE_FULL_FILE  
  
  ; Size of the screen
  size_sc = GET_SCREEN_SIZE()
  
  ; Font
  fon = 'TIMES'

  ; Create a base widget
  WID_BASE_0 = Widget_Base(GROUP_LEADER=wGroup, UNAME='WID_BASE_0'  $
      ,XOFFSET = 5 ,YOFFSET = 5 , SCR_XSIZE = size_sc[0], SCR_YSIZE = size_sc[1]  $
      ,NOTIFY_REALIZE='StartTop' ,KILL_NOTIFY='EndTop', TITLE = 'The'+ $
      ' bisector' + ' method', SPACE = 3, XPAD = 3, YPAD = 3, MBAR = WID_BASE_0_MBAR, /SCROLL)

  ; Attach buttons
  W_MENU_0 = Widget_Button(WID_BASE_0_MBAR, UNAME='W_MENU_0' ,/MENU, VALUE='Menu')

  RV = Widget_Button(WID_BASE_0_MBAR, UNAME='RV' ,/MENU, VALUE='Radial velocities')
  
  RV_1 = Widget_Button(RV, UNAME='RV_1', VALUE='Load file with radial velocities')
  
  Corr = Widget_Button(WID_BASE_0_MBAR, UNAME='Corr', /MENU, VALUE='Correlation')
  
  Corr_0 = Widget_Button(Corr, UNAME='Corr_0', VALUE='Display resulting BVS and RV')
  
  Corr_1 = Widget_Button(Corr, UNAME='Corr_1', VALUE='Compute correlation')
      
  W_MENU_1 = Widget_Button(W_MENU_0, UNAME='W_MENU_1', VALUE='Load data files...')
  
  W_MENU_2 = Widget_Button(W_MENU_0, UNAME='W_MENU_2', VALUE='Add data files...')
  
  W_MENU_3 = Widget_Button(W_MENU_0, UNAME='W_MENU_3', VALUE='Settings')
  
  W_MENU_9 = Widget_Button(W_MENU_0, UNAME='W_MENU_9', /SEPARATOR, VALUE='Help')
  
  W_MENU_4 = Widget_Button(W_MENU_0, UNAME='W_MENU_4', /SEPARATOR, VALUE='Exit')
      
  ; Variables for size settings
  sl_width = 0.2 * size_sc[0]
  sl_high  = 0.1 * size_sc[1] 

  ; Widgets
  WID_Tab = Widget_Base(WID_BASE_0, UNAME='WID_Tab', XOFFSET = 0.01 * size_sc[0] + 0.1838 * size_sc[0] $
    , YOFFSET=0.14 * size_sc[1] , XSIZE= 0.765 * size_sc[0], YSIZE= 0.68 * size_sc[1], frame = 1)
  
  Table = Widget_Table(WID_Tab, UNAME = 'Table', /SCROLL, BACKGROUND_COLOR = [246, 222, 190], COLUMN_LABELS = ['BVS', 'RV'], XOFFSET = 0 $
      , YOFFSET = 0, SCR_XSIZE = size_sc[0], SCR_YSIZE = size_sc[1], frame = 1, FONT = fon, FOREGROUND_COLOR = [127, 0, 127] $
      , /NO_ROW_HEADERS, X_SCROLL_SIZE = size_sc[0], Y_SCROLL_SIZE = size_sc[1], /RESIZEABLE_COLUMNS)
      
  Calculate_bisec = Widget_Button(WID_BASE_0, UNAME='Calculate_bisec', VALUE='Calculate bisector', FONT = fon $
      , XOFFSET= 0.01 * size_sc[0] + 0.1838 * size_sc[0], YOFFSET = 0.0052 * size_sc[1], XSIZE = 0.096 * size_sc[0], YSIZE = 1.27 * sl_high)
  
  Calculate_BVS = Widget_Button(WID_BASE_0, UNAME='Calculate_BVS', VALUE='Calculate BVS', FONT = fon $
      , XOFFSET= 0.848 * size_sc[0], YOFFSET = 0.0052 * size_sc[1], XSIZE = 0.11 * size_sc[0], YSIZE = 1.55 * sl_high/2)
  
  BVS = Widget_Text(WID_BASE_0, UNAME='BVS', XOFFSET= 0.848 * size_sc[0], YOFFSET =  0.093 * size_sc[1] $
    , XSIZE = 0.0132 * size_sc[0], YSIZE = 1, FONT = fon, VALUE = 'BVS = ?')

  ; Attach draw widget
  WID_DRAW_0 = Widget_Window(WID_BASE_0, UNAME='WID_DRAW_0', XOFFSET = 0.01 * size_sc[0] + 0.1838 * size_sc[0] $
      , YOFFSET=0.14 * size_sc[1] , XSIZE= 0.765 * size_sc[0], YSIZE= 0.68 * size_sc[1], frame = 1)

  Label1 = Widget_label(WID_BASE_0, value = 'Choose intensities of belts for computing bisector velocity span (BVS):' $
    , ysize = 0.026 * size_sc[1], XOFFSET = 10 + 0.195 * size_sc[0] + 0.096 * size_sc[0], YOFFSET = 0.0052 * size_sc[1], FONT = fon)
   
  Buttom_CW_FSL = Widget_Base(WID_BASE_0, UNAME='Buttom_CW_FSL'  $
    , XOFFSET=10+0.01 * size_sc[0] + 0.1838 * size_sc[0] + 0.096 * size_sc[0], YOFFSET = 30 , SCR_XSIZE = sl_width + 20 ,SCR_YSIZE = sl_high + 20)
  
  SLIDER1 = CW_FSLIDER(Buttom_CW_FSL, UNAME='SLIDER1', TITLE='Intensity of bottom part (% from I0)', /EDIT, FORMAT = '(G10.3)' $
    , XSIZE = sl_width, YSIZE = sl_high, FRAME = 1, MAXIMUM = 50, MINIMUM = 0)

  Buttom_CW_FSL_2 = Widget_Base(WID_BASE_0, UNAME='Buttom_CW_FSL'  $
    , XOFFSET=10+0.01 * size_sc[0] + sl_width + 20 + 0.1838 * size_sc[0] + 0.096 * size_sc[0],YOFFSET=30, SCR_XSIZE=sl_width+20 ,SCR_YSIZE=sl_high+40)

  SLIDER2 = CW_FSLIDER(Buttom_CW_FSL_2, UNAME='SLIDER2', TITLE='Intensity of top part (% from IC)', /EDIT, FORMAT = '(G10.3)' $
    , XSIZE=sl_width, YSIZE=sl_high, FRAME = 1, MAXIMUM = 50, MINIMUM = 0)
  
  Label3 = Widget_label(WID_BASE_0, value = 'Width of belts (%):', ysize = 0.026 * size_sc[1], XOFFSET = 0.728 * size_sc[0] $
    , YOFFSET = 0.068 * size_sc[1], FONT = fon)

  STEP = Widget_Text(WID_BASE_0, UNAME='STEP', /EDITABLE, XOFFSET = 0.727 * size_sc[0], YOFFSET = 0.093 * size_sc[1] $
    , XSIZE=18 ,YSIZE=1, FONT = fon, VALUE = '0')
  
  Label4 = Widget_label(WID_BASE_0, value = 'Central wavelength (nm):', ysize = 0.026 * size_sc[1], XOFFSET = 0.728 * size_sc[0] $
    , YOFFSET = 0.0052 * size_sc[1], FONT = fon)

  lambda_0 = Widget_Text(WID_BASE_0, UNAME='lambda_0', /EDITABLE, XOFFSET = 0.727 * size_sc[0], YOFFSET = 0.03 * size_sc[1] $
    , XSIZE=18 ,YSIZE=1, FONT = fon, VALUE = '0')
    
  Label2 = Widget_label(WID_BASE_0, value = 'Double-click to plot the profile line:', ysize = 20, XOFFSET= 10 $
    , YOFFSET= 0.0052 * size_sc[1], FONT = fon)

  id_list_0 = Widget_list(WID_BASE_0, VALUE = shortname, YSIZE = 0.058 * size_sc[1], XSIZE = 0.029 * size_sc[0] $
    , XOFFSET = 0.0073 * size_sc[0], YOFFSET = 0.0391 * size_sc[1], UNAME='W_LIST_0', frame = 1)
    
  Remove = Widget_Button(WID_BASE_0, UNAME='Remove', VALUE = 'Remove file', XOFFSET = 0.0074 * size_sc[0] $
    , YOFFSET = 0.8 * size_sc[1], XSIZE = 0.16 * size_sc[0], YSIZE = 0.0351 * size_sc[1], FONT = fon)

  AllBVS = Widget_Button(WID_BASE_0, UNAME='AllBVS', VALUE = 'Compute BVS for all files', XOFFSET = 0.0074 * size_sc[0] $
    , YOFFSET = 0.84 * size_sc[1], XSIZE = 0.16 * size_sc[0], YSIZE = 0.0351 * size_sc[1], FONT = fon)
      
  ; Plot widget for information
  WID_DRAW_ERR = Widget_Window(WID_BASE_0, UNAME = 'WID_DRAW_ERR', XOFFSET = 0.01 * size_sc[0] + 0.1838 * size_sc[0]$
    , YOFFSET = 0.81 * size_sc[1], XSIZE = 0.766 * size_sc[0], YSIZE = 100)
  
  ; Table is not always displayed
  widget_control, WID_Tab, MAP = 0
       
  ; Realize the widgets
  Widget_Control, /REALIZE, WID_BASE_0
  ; Call XMANAGER to manage the widgets
  XManager, 'WID_BASE_0', WID_BASE_0, /NO_BLOCK  ,CLEANUP='EndTop'  

end

;-------------------------------------------------------------------------------------------------------------------------------------------------
; Empty stub procedure used for autoloading.
;-------------------------------------------------------------------------------------------------------------------------------------------------
pro widgets, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
  WID_BASE_0, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
  
end