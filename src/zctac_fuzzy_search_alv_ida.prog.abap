REPORT ZCTAC_FUZZY_SEARCH_ALV_IDA.
*/**
* Example: Fuzzy search on a text column and fuzzy search in the ALV output
*/
DATA: wa_vbap TYPE vbap.

DATA lo_docking_container TYPE REF TO cl_gui_docking_container.

SELECT-OPTIONS s_vbeln FOR wa_vbap-vbeln.

PARAMETERS sterm TYPE string VISIBLE LENGTH 18 LOWER CASE .
PARAMETERS simil TYPE p LENGTH 2 DECIMALS 1 DEFAULT '0.8' .

INITIALIZATION.

  IF    cl_salv_gui_table_ida=>db_capabilities( )->is_text_search_supported( ) = abap_false.
    MESSAGE 'Text search not supported'  TYPE 'E'.
  ENDIF.

* Gebruik hier NIET start-of-selection maar AT SELECTION-SCREEN OUTPUT.
* Door de Docking Container krijg je selectiescherm + de output in een ALV

AT SELECTION-SCREEN OUTPUT. " !!!

  DATA(row_size_in_pixel) = 18.
  DATA(extension) = sy-srows * row_size_in_pixel * ( 80 / 100 ).
  IF lo_docking_container IS NOT BOUND.
    lo_docking_container = NEW #( repid     = sy-repid
                                  dynnr     = sy-dynnr
                                  side      = cl_gui_docking_container=>dock_at_bottom
                                  metric    = cl_gui_docking_container=>metric_pixel
                                  extension = extension ).
    DATA(lo_alv_display) = cl_salv_gui_table_ida=>create( iv_table_name    = 'VBAP'
                                                          io_gui_container = lo_docking_container ).

    " Select Options
    DATA(lo_collector) = NEW cl_salv_range_tab_collector( ).
    lo_collector->add_ranges_for_name( iv_name = 'VBELN'      it_ranges = s_vbeln[] ).
    lo_collector->get_collected_ranges( IMPORTING et_named_ranges = DATA(lt_name_range_pairs) ).
    lo_alv_display->set_select_options( it_ranges = lt_name_range_pairs ).

    "field catalog
    DATA: lts_field_name TYPE if_salv_gui_types_ida=>yts_field_name.
    INSERT CONV #( 'VBELN' ) INTO TABLE lts_field_name.
    INSERT CONV #( 'POSNR' ) INTO TABLE lts_field_name.
    INSERT CONV #( 'MATNR' ) INTO TABLE lts_field_name.
    INSERT CONV #( 'ARKTX' ) INTO TABLE lts_field_name.
    lo_alv_display->field_catalog( )->set_available_fields( EXPORTING its_field_names = lts_field_name ).

    "Search Columns
    DATA: lts_field_name2 TYPE if_salv_gui_types_ida=>yts_field_name.
    INSERT CONV #( 'ARKTX' ) INTO TABLE lts_field_name2.
    lo_alv_display->text_search( )->set_search_scope( lts_field_name2 ).

    IF sterm IS NOT INITIAL.
      lo_alv_display->text_search( )->set_field_similarity( simil ).
    ENDIF.
    lo_alv_display->text_search( )->set_search_term( sterm ).

  ENDIF.
