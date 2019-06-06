REPORT zctac_find_wb_via_description.
*/**
*&---------------------------------------------------------------------*
*& Author           | Ben Meijs Ctac
*& Date             | 24.05.2019 13:39:06
*& Release          | ECC 740 SP5 vv
*& Version          | V1
*& Released for use | 24-05-2019
*&---------------------------------------------------------------------*
*$ Purpose: Searches for all kinds of WB objects via description.
*$
*$ -> Use @ to explicitly search for space
*$ -> Use #* to search for *
*$ -> The search string is translated into a select option CP *search string*.
*$ -> Triple Search: search for the original search string + three extra alternatives
*$      1) search string in all lower case
*$      2) search string in all upper case
*$      3) search string every separate word starts with capital in all upper case
*$
*$
*& NOTES: Use on a HANA system for very fast result.
*&
*&---------------------------------------------------------------------*
*& DB-Changes/LUW   :  N.A.
*&
*&---------------------------------------------------------------------*
*/

TYPES: BEGIN OF ty_st_result,
         main_object   TYPE tadir-object,
         main_obj_name TYPE tadir-obj_name,
         sub_object    TYPE e071-object,
         sub_obj_name  TYPE e071-obj_name,
         language      TYPE sy-langu,
         description   TYPE string,
         txttab        TYPE dd02l-tabname,
         devclass      TYPE tadir-devclass,
       END OF ty_st_result,
       ty_ta_result TYPE STANDARD TABLE OF ty_st_result WITH DEFAULT KEY.

DATA: ta_result TYPE ty_ta_result.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(35) txt_srch.
PARAMETERS: pa_srch  TYPE string OBLIGATORY LOWER CASE.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(35) txt_prog.
PARAMETERS: cb_prog  AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(35) txt_ddic.
PARAMETERS:             cb_ddic  AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(35) txt_enha.
PARAMETERS:             cb_enha  AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(35) txt_form.
PARAMETERS:             cb_forms AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(35) txt_web.
PARAMETERS:             cb_web   AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF LINE.



SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(35) txt_trip.
PARAMETERS:             cb_tripl AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT (35) txt_tri2.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(35) txt_lnga.
PARAMETERS:             rb_langa RADIOBUTTON GROUP rad1 DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(35) txt_lngu.
PARAMETERS:             rb_langu RADIOBUTTON GROUP rad1.
SELECTION-SCREEN COMMENT 40(10) txt_lngp.
PARAMETERS:             pa_langu TYPE sy-langu DEFAULT sy-langu.
SELECTION-SCREEN END OF LINE.

DATA: soname TYPE string VALUE 'TADIR-DEVCLASS'.
SELECT-OPTIONS: so_devcl FOR (soname).

INITIALIZATION.
  txt_srch = 'Zoekstring (@ = spatie, #* = *)'.
  txt_prog = '...in Programmaobjecten'.
  txt_ddic = '...in DDIC objecten'.
  txt_enha = '...in Uitbreidingen'.
  txt_form = '...in formulieren'.
  txt_web  = '...in WEB objecten'.
  txt_lnga = 'In alle talen'.
  txt_lngu = 'In opgegeven taal'.
  txt_lngp = 'Taalcode'.
  txt_trip = 'Triple Search'.
  txt_tri2 = 'woord/Woord/WOORD'.

AT SELECTION-SCREEN.
  IF cb_prog  = abap_false AND
     cb_ddic  = abap_false AND
     cb_enha  = abap_false AND
     cb_forms = abap_false AND
     cb_web   = abap_false .
    MESSAGE 'Je moet minimaal één zoekcategorie selecteren'(e01) TYPE 'E'.
  ENDIF.

  IF pa_srch = '*'.
    MESSAGE 'Zoek op alles. Niet toegestaan (#*)' TYPE 'E'.
  ENDIF.

START-OF-SELECTION.
  DATA(tp_srch) = pa_srch.
  DATA: search_range TYPE RANGE OF string.
  IF cb_tripl = abap_false.
    REPLACE ALL OCCURRENCES OF '@' IN tp_srch WITH ` ` .
    search_range = VALUE #( ( low = |*{ tp_srch }*|
                              option = 'CP'
                              sign = 'I' ) ).

  ELSE.
    DATA(tp_srch_split) = tp_srch.

    SPLIT tp_srch_split AT '@' INTO TABLE DATA(lta_split).
    LOOP AT lta_split ASSIGNING FIELD-SYMBOL(<split>).
      CASE strlen( <split> ).
        WHEN 0.
        WHEN 1.
          <split> = to_upper( <split>(1) ).
        WHEN OTHERS.
          <split> = to_upper( <split>(1) ) && to_lower( <split>+1 ).
      ENDCASE.
    ENDLOOP.
    CONCATENATE LINES OF lta_split INTO tp_srch_split SEPARATED BY '@'.
    REPLACE ALL OCCURRENCES OF '@' IN tp_srch WITH ` ` .
    REPLACE ALL OCCURRENCES OF '@' IN tp_srch_split WITH ` ` .

    IF strlen( tp_srch ) > strlen( tp_srch_split ).
      "must be because there is now a space at the end because there was a @ at the end. "add the space
      tp_srch_split = tp_srch_split && ` `.
    ENDIF.

    search_range = VALUE #( ( low = |*{ tp_srch }*|                    "the word itself
                              option = 'CP'
                              sign = 'I' )
                            ( low = |*{ to_lower( tp_srch ) }*|        "the word in lower case
                              option = 'CP'
                              sign = 'I' )
                            ( low = |*{ to_upper( tp_srch ) }*|        "the word in upper case
                              option = 'CP'
                              sign = 'I' )
                            ( low = |*{ tp_srch_split  }*|
                              option = 'CP'
                              sign = 'I' )

                              ).



  ENDIF.
  DATA(language) = COND #( WHEN rb_langa = abap_true THEN '%' ELSE pa_langu ).




  IF cb_prog = abap_true.
    PERFORM search_prog_objects.
  ENDIF.
  IF cb_enha = abap_true.
    PERFORM search_enha_objects.
  ENDIF.
  IF cb_web = abap_true.
    PERFORM search_web_objects.
  ENDIF.
  IF cb_forms = abap_true.
    PERFORM search_forms_objects.
  ENDIF.

  IF cb_ddic = abap_true.
    PERFORM search_ddic_objects.
  ENDIF.

  SORT ta_result BY main_object main_obj_name sub_object sub_obj_name.

  LOOP AT ta_result
    ASSIGNING FIELD-SYMBOL(<result>)
    WHERE ( main_object = 'PROG' AND main_obj_name CS 'SAPL' ) OR
                          ( main_object = 'FUGR' AND main_obj_name CS 'SAPL' ).
    REPLACE 'SAPL' WITH `` INTO <result>-main_obj_name.
*    shift <result
    <result>-main_object = 'FUGR'.
    SELECT SINGLE devclass FROM tadir INTO <result>-devclass WHERE pgmid = 'R3TR' AND object = <result>-main_object AND obj_name = <result>-main_obj_name.
  ENDLOOP.
  LOOP AT ta_result
    ASSIGNING <result>
    WHERE ( main_object = 'PROG' AND main_obj_name+30(2) = 'CP' ).
    <result>-main_object = 'CLAS'.
    <result>-main_obj_name = <result>-main_obj_name(30).
    TRANSLATE <result>-main_obj_name USING '= '.

  ENDLOOP.
  LOOP AT ta_result ASSIGNING <result> WHERE devclass IS INITIAL.
    SELECT SINGLE devclass FROM tadir INTO <result>-devclass WHERE pgmid = 'R3TR' AND object = <result>-main_object AND obj_name = <result>-main_obj_name.
  ENDLOOP.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table   = DATA(alv)
        CHANGING
          t_table        = ta_result
      ).
      alv->get_functions( )->set_all( ).
      alv->get_display_settings( )->set_striped_pattern( cl_salv_display_settings=>true ).
      alv->get_display_settings( )->set_list_header( sy-title ).

      DATA(columns) = alv->get_columns( ).
      columns->set_optimize( ).
      DATA(columns_tab) = alv->get_columns( )->get( ).
      LOOP AT columns_tab ASSIGNING FIELD-SYMBOL(<column>).
        IF <column>-r_column->get_long_text( ) IS INITIAL.
          <column>-r_column->set_long_text( CONV #( <column>-columnname ) ).
        ENDIF.
      ENDLOOP.

      alv->display( ).

    CATCH cx_salv_msg INTO DATA(alv_exc).
      MESSAGE alv_exc->get_text( ) TYPE 'E'.
  ENDTRY.
*&---------------------------------------------------------------------*
*&      Form  SEARCH_PROG_OBJECTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM search_prog_objects .
* Packages
  SELECT 'DEVC' AS main_object,
         tdevct~devclass AS main_obj_name,
         tdevct~spras AS language,
         tdevct~ctext AS description,
         'TDEVCT' AS txttab
        FROM tdevct
        APPENDING CORRESPONDING FIELDS OF TABLE @ta_result
        WHERE ctext   IN @search_range
          AND spras LIKE @language.
* transacties TSTCT
  SELECT 'TRAN' AS main_object,
         tstct~tcode AS main_obj_name,
         tstct~sprsl AS language,
         tstct~ttext AS description,
         'TSTCT' AS txttab
        FROM tstct APPENDING CORRESPONDING FIELDS OF TABLE @ta_result
        WHERE ttext IN @search_range
          AND sprsl LIKE @language.


* programma's TRDIRT
  SELECT
         'PROG' AS main_object,
         trdirt~name AS main_obj_name,
         CASE trdir~subc
          WHEN '1'  THEN 'REPO'
          WHEN 'I'  THEN 'INCL'
          WHEN 'M'  THEN 'MODP'
          WHEN 'S'  THEN 'SUBR'
          WHEN 'K'  THEN 'CLSP'
          WHEN 'J'  THEN 'INFP'
          WHEN 'F'  THEN 'FUGR'
          ELSE trdir~subc
          END AS sub_object,
         trdirt~sprsl AS language,
         trdirt~text AS description,

         'TRDIRT' AS txttab,
         tadir~devclass
        FROM trdirt INNER JOIN trdir ON trdir~name = trdirt~name
          LEFT OUTER JOIN tadir ON tadir~pgmid = 'R3TR'  AND tadir~object = 'PROG' AND tadir~obj_name = trdir~name
    APPENDING CORRESPONDING FIELDS OF TABLE @ta_result
        WHERE text IN @search_range
          AND sprsl LIKE @language.

  SELECT CASE seoclass~clstype
          WHEN '0' THEN 'CLAS'
          WHEN '1' THEN 'INTF'
         END              AS main_object,
         seoclasstx~clsname  AS main_obj_name,
         seoclasstx~langu    AS language,
         seoclasstx~descript AS description,
         'SEOCLASSTX' AS txttab,
         CASE WHEN tadir~devclass IS NOT NULL
           THEN tadir~devclass
           WHEN tadir2~devclass IS NOT NULL
           THEN tadir2~devclass
         END AS devclass
        FROM seoclasstx INNER JOIN seoclass ON seoclass~clsname = seoclasstx~clsname
        LEFT OUTER JOIN tadir ON tadir~pgmid = 'R3TR'  AND tadir~object = 'CLAS' AND tadir~obj_name = seoclass~clsname
        LEFT OUTER JOIN tadir AS tadir2 ON tadir2~pgmid = 'R3TR'  AND tadir2~object = 'INTF' AND tadir2~obj_name = seoclass~clsname

    APPENDING CORRESPONDING FIELDS OF TABLE @ta_result
        WHERE descript IN @search_range
          AND langu    LIKE @language.

  SELECT CASE seoclass~clstype
          WHEN '0' THEN 'CLAS'
          WHEN '1' THEN 'INTF'
         END              AS main_object,
         seocompotx~clsname  AS main_obj_name,
         CASE seocompo~cmptype
          WHEN '0' THEN 'ATTR'
          WHEN '1' THEN 'METH'
          WHEN '2' THEN 'EVNT'
          WHEN '3' THEN 'TYPE'
          ELSE     '????'
         END AS sub_object,
         seocompotx~cmpname  AS sub_obj_name,
         seocompotx~langu    AS language,
         seocompotx~descript AS description,
         'SEOCOMPOTX' AS txttab,
         CASE WHEN tadir~devclass IS NOT NULL
           THEN tadir~devclass
           WHEN tadir2~devclass IS NOT NULL
           THEN tadir2~devclass
         END AS devclass
        FROM seocompotx
           INNER JOIN seoclass ON seoclass~clsname = seocompotx~clsname
           INNER JOIN seocompo ON seocompo~clsname = seocompotx~clsname
                              AND seocompo~cmpname = seocompotx~cmpname
        LEFT OUTER JOIN tadir ON tadir~pgmid = 'R3TR'  AND tadir~object = 'CLAS' AND tadir~obj_name = seoclass~clsname
        LEFT OUTER JOIN tadir AS tadir2 ON tadir2~pgmid = 'R3TR'  AND tadir2~object = 'INTF' AND tadir2~obj_name = seoclass~clsname
    APPENDING CORRESPONDING FIELDS OF TABLE @ta_result
        WHERE descript IN @search_range
          AND langu    LIKE @language.



* Functiegroepen TLIBT
*SPRAS
*AREA
*AREAT
  SELECT 'FUGR'              AS  main_object,
         tlibt~area          AS main_obj_name,
         tlibt~spras         AS language,
         tlibt~areat         AS description,
         'TLIBT' AS txttab,
        tadir~devclass
        FROM tlibt
        LEFT OUTER JOIN tadir ON tadir~pgmid = 'R3TR'  AND tadir~object = 'FUGR' AND tadir~obj_name = tlibt~area
    APPENDING CORRESPONDING FIELDS OF TABLE @ta_result
        WHERE areat IN @search_range
          AND spras LIKE @language.
* Functiebouwstenen TFTIT
  SELECT 'FUGR'              AS  main_object,
         tfdir~pname         AS main_obj_name,
         'FUNC'              AS sub_object,
         tfdir~funcname      AS sub_obj_name,
         tftit~spras         AS language,
         tftit~stext         AS description,
         'TFTIT' AS txttab
        FROM tftit INNER JOIN tfdir ON tfdir~funcname = tftit~funcname
    APPENDING CORRESPONDING FIELDS OF TABLE @ta_result
        WHERE stext IN @search_range
          AND spras LIKE @language.
* Dynpro's
  SELECT 'PROG'              AS  main_object,
         d020t~prog          AS main_obj_name,
         'DYNP'              AS sub_object,
         d020t~dynr          AS sub_obj_name,
         d020t~lang          AS language,
         d020t~dtxt          AS description,
         'D020T' AS txttab,
        tadir~devclass
        FROM d020t
          LEFT OUTER JOIN tadir ON tadir~pgmid = 'R3TR'  AND tadir~object = 'PROG' AND tadir~obj_name = d020t~prog
    APPENDING CORRESPONDING FIELDS OF TABLE @ta_result
        WHERE dtxt IN @search_range
          AND lang LIKE @language.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SEARCH_ENHA_OBJECTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM search_enha_objects .
  SELECT 'SXCI'              AS  main_object,
         sxc_attrt~imp_name          AS main_obj_name,
         sxc_attrt~sprsl         AS language,
         sxc_attrt~text          AS description,
         'sxc_attrt' AS txttab
        FROM sxc_attrt
    APPENDING CORRESPONDING FIELDS OF TABLE @ta_result
        WHERE text IN @search_range
          AND sprsl LIKE @language.

  SELECT 'SXSD'              AS  main_object,
         sxs_attrt~exit_name          AS main_obj_name,
         sxs_attrt~sprsl         AS language,
         sxs_attrt~text          AS description,
         'sxs_attrt' AS txttab
        FROM sxs_attrt
    APPENDING CORRESPONDING FIELDS OF TABLE @ta_result
        WHERE text IN @search_range
          AND sprsl LIKE @language.

  SELECT 'ENHC'                  AS  main_object,
         src~enhcomposite        AS main_obj_name,
         sotr_text~langu         AS language,
         sotr_text~text          AS description,
         'sotr_text' AS txttab
        FROM enhcompheader AS src INNER JOIN sotr_text ON sotr_text~concept = src~shorttextid
    APPENDING CORRESPONDING FIELDS OF TABLE @ta_result
        WHERE text IN @search_range
          AND langu LIKE @language.

  SELECT 'ENHO'                  AS  main_object,
         src~enhname             AS main_obj_name,
         sotr_text~langu         AS language,
         sotr_text~text          AS description,
         'sotr_text' AS txttab
        FROM enhheader AS src INNER JOIN sotr_text ON sotr_text~concept = src~shorttext_id
    APPENDING CORRESPONDING FIELDS OF TABLE @ta_result
        WHERE text IN @search_range
          AND langu LIKE @language.

  SELECT 'ENHS'                  AS  main_object,
         src~enhspot             AS main_obj_name,
         sotr_text~langu         AS language,
         sotr_text~text          AS description,
         'sotr_text' AS txttab
        FROM enhspotheader AS src INNER JOIN sotr_text ON sotr_text~concept = src~shorttextid
    APPENDING CORRESPONDING FIELDS OF TABLE @ta_result
        WHERE text IN @search_range
          AND langu LIKE @language.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SEARCH_WEB_OBJECTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM search_web_objects .
  SELECT 'WDYN'              AS  main_object,
         src~component_name          AS main_obj_name,
         src~langu         AS language,
         src~description          AS description,
         'wdy_componentt' AS txttab
        FROM wdy_componentt AS src
    APPENDING CORRESPONDING FIELDS OF TABLE @ta_result
        WHERE description IN @search_range
          AND langu     LIKE @language.

  SELECT 'WDYA'              AS  main_object,
        src~application_name          AS main_obj_name,
        src~langu         AS language,
        src~description          AS description,
        'wdy_applicationt' AS txttab
       FROM wdy_applicationt AS src
   APPENDING CORRESPONDING FIELDS OF TABLE @ta_result
       WHERE description IN @search_range
         AND langu     LIKE @language.

  SELECT 'WAPA'              AS  main_object,
        src~applname          AS main_obj_name,
        src~langu         AS language,
        src~text          AS description,
        'o2applt' AS txttab
       FROM o2applt AS src
   APPENDING CORRESPONDING FIELDS OF TABLE @ta_result
       WHERE text IN @search_range
         AND langu     LIKE @language.

  SELECT 'SICF'              AS  main_object,
        src~icf_name && src~icfparguid          AS main_obj_name,
        src~icf_langu         AS language,
        src~icf_docu          AS description,
        'icfdocu' AS txttab
       FROM icfdocu AS src
   APPENDING CORRESPONDING FIELDS OF TABLE @ta_result
       WHERE icf_docu IN @search_range
         AND icf_langu     LIKE @language.

  SELECT 'WEBI'                  AS  main_object,
         src~vepname        AS main_obj_name,
         sotr_text~langu         AS language,
         sotr_text~text          AS description,
         'sotr_text' AS txttab
        FROM vepheader AS src INNER JOIN sotr_text ON sotr_text~concept = src~text_id
    APPENDING CORRESPONDING FIELDS OF TABLE @ta_result
        WHERE text IN @search_range
          AND langu LIKE @language.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SEARCH_FORMS_OBJECTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM search_forms_objects .
  SELECT 'FORM'              AS  main_object,
        src~tdname          AS main_obj_name,
        src~tdspras         AS language,
        src~tdtitle          AS description,
        'stxh' AS txttab
       FROM stxh AS src
   APPENDING CORRESPONDING FIELDS OF TABLE @ta_result
       WHERE tdtitle IN @search_range
         AND tdobject = 'FORM'
         AND tdid     = 'TXT'
         AND tdspras     LIKE @language.
  SELECT 'STYL'              AS  main_object,
        src~tdname          AS main_obj_name,
        src~tdspras         AS language,
        src~tdtitle          AS description,
        'stxh' AS txttab
       FROM stxh AS src
   APPENDING CORRESPONDING FIELDS OF TABLE @ta_result
       WHERE tdtitle IN @search_range
         AND tdobject = 'STYLE'
         AND tdid     = 'TXT'
         AND tdspras     LIKE @language.


  SELECT 'SSFO'              AS  main_object,
        src~formname          AS main_obj_name,
        src~langu         AS language,
        src~caption          AS description,
        'stxfadmt' AS txttab
       FROM stxfadmt AS src
   APPENDING CORRESPONDING FIELDS OF TABLE @ta_result
       WHERE caption IN @search_range
         AND langu     LIKE @language.

  SELECT 'SSST'              AS  main_object,
        src~stylename          AS main_obj_name,
        src~langu         AS language,
        src~caption          AS description,
        'stxsadmt' AS txttab
       FROM stxsadmt AS src
   APPENDING CORRESPONDING FIELDS OF TABLE @ta_result
       WHERE caption IN @search_range
         AND langu     LIKE @language.

  SELECT 'SFPF'              AS  main_object,
        src~name          AS main_obj_name,
        src~language         AS language,
        src~text          AS description,
        'fpcontextt' AS txttab
       FROM fpcontextt AS src
   APPENDING CORRESPONDING FIELDS OF TABLE @ta_result
       WHERE text IN @search_range
         AND language     LIKE @language
         AND state = 'A'
         AND id = ''.    .

  SELECT 'SFPI'              AS  main_object,
        src~name          AS main_obj_name,
        src~language         AS language,
        src~text          AS description,
        'fpinterfacet' AS txttab
       FROM fpinterfacet AS src
   APPENDING CORRESPONDING FIELDS OF TABLE @ta_result
       WHERE text IN @search_range
         AND language     LIKE @language
         AND state = 'A'
         AND id = ''.    .
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SEARCH_DDIC_OBJECTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM search_ddic_objects .
* tabellen/structuren
  SELECT 'TABL'              AS  main_object,
         dd02l~tabname       AS main_obj_name,
    CASE dd02l~tabclass
        WHEN 'TRANSP' THEN 'TABL'
        WHEN 'INTTAB' THEN 'STRU'
        WHEN 'CLUSTER' THEN 'CLUS'
        WHEN 'POOL'   THEN 'POOL'
       WHEN 'VIEW' THEN 'VIEW'
       WHEN 'APPEND' THEN 'APPEND'
       ELSE dd02l~tabclass
    END AS sub_object,
         dd02t~ddlanguage    AS language,
         dd02t~ddtext        AS description,
       'DD02T' AS txttab,
      tadir~devclass
        FROM dd02t INNER JOIN dd02l  ON dd02l~tabname = dd02t~tabname
        LEFT OUTER JOIN tadir ON tadir~pgmid = 'R3TR'  AND tadir~object = 'TABL' AND tadir~obj_name = dd02l~tabname

    APPENDING CORRESPONDING FIELDS OF TABLE @ta_result
        WHERE ddtext     IN @search_range
          AND ddlanguage LIKE @language.
* CDS DDLS
  SELECT 'DDLS'              AS  main_object,
   ddddlsrct~ddlname       AS main_obj_name,
   ddddlsrct~ddlanguage    AS language,
   ddddlsrct~ddtext        AS description,
 'DDDDLSRCT' AS txttab
  FROM ddddlsrct
APPENDING CORRESPONDING FIELDS OF TABLE @ta_result
  WHERE ddtext     IN @search_range
    AND ddlanguage LIKE @language.

* Views DD25L/T
*V  View
*E  Lock object
*M  Matchcode object
  SELECT
        CASE dd25l~aggtype
    WHEN 'V' THEN 'VIEW'
    WHEN 'E' THEN 'ENQU'
    WHEN 'M' THEN 'MCOB'

    END
                AS  main_object,
   dd25t~viewname      AS main_obj_name,
    CASE dd25l~aggtype
    WHEN 'V' THEN 'VIEW'
    WHEN 'E' THEN 'ENQU'
    WHEN 'M' THEN 'MCOB'
    ELSE dd25l~aggtype
    END AS   sub_object,

   dd25t~ddlanguage    AS language,
   dd25t~ddtext        AS description,
 'DD25T' AS txttab
  FROM dd25t INNER JOIN dd25l ON dd25l~viewname = dd25t~viewname
 APPENDING CORRESPONDING FIELDS OF TABLE @ta_result
  WHERE ddtext     IN @search_range
    AND ddlanguage LIKE @language.

* Dataelementen
  SELECT 'DTEL'              AS  main_object,
         dd04l~rollname       AS main_obj_name,
         dd04t~ddlanguage    AS language,
         dd04t~ddtext        AS description,
       'DD04T' AS txttab
        FROM dd04t INNER JOIN dd04l  ON dd04l~rollname = dd04t~rollname
    APPENDING CORRESPONDING FIELDS OF TABLE @ta_result
        WHERE ddtext     IN @search_range
          AND ddlanguage LIKE @language.
* domeinen
  SELECT 'DOMA'              AS  main_object,
         dd01l~domname        AS main_obj_name,
         dd01t~ddlanguage    AS language,
         dd01t~ddtext        AS description,
       'DD01T' AS txttab
        FROM dd01t INNER JOIN dd01l  ON dd01l~domname = dd01t~domname
    APPENDING CORRESPONDING FIELDS OF TABLE @ta_result
        WHERE ddtext     IN @search_range
          AND ddlanguage LIKE @language.
* Search Helps
  SELECT 'SHLP'              AS  main_object,
         dd30t~shlpname        AS main_obj_name,
         dd30t~ddlanguage    AS language,
         dd30t~ddtext        AS description,
       'DD30T' AS txttab
        FROM dd30t
    APPENDING CORRESPONDING FIELDS OF TABLE @ta_result
        WHERE ddtext     IN @search_range
          AND ddlanguage LIKE @language.

* Table tYPES
  SELECT 'TTYP'              AS  main_object,
         dd40t~typename        AS main_obj_name,
         dd40t~ddlanguage    AS language,
         dd40t~ddtext        AS description,
       'DD40T' AS txttab
        FROM dd40t
    APPENDING CORRESPONDING FIELDS OF TABLE @ta_result
        WHERE ddtext     IN @search_range
          AND ddlanguage LIKE @language.
* Type Groups
  SELECT 'TYPE'             AS main_object,
         ddtypet~typegroup  AS main_obj_name,
         ddtypet~ddlanguage AS language,
         ddtypet~ddtext     AS description,
         'DDTYPET'          AS txttab
         FROM ddtypet
    APPENDING CORRESPONDING FIELDS OF TABLE @ta_result
        WHERE ddtext     IN @search_range
          AND ddlanguage LIKE @language.

ENDFORM.
