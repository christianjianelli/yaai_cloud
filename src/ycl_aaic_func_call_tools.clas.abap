CLASS ycl_aaic_func_call_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_aaic_func_call_tools.

    INTERFACES if_oo_adt_classrun.

    ALIASES get_available_tools FOR yif_aaic_func_call_tools~get_available_tools.
    ALIASES request_tools FOR yif_aaic_func_call_tools~request_tools.

    METHODS constructor
      IMPORTING
        i_o_agent TYPE REF TO yif_aaic_agent OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA _o_agent TYPE REF TO yif_aaic_agent.

ENDCLASS.



CLASS ycl_aaic_func_call_tools IMPLEMENTATION.

  METHOD constructor.

    IF i_o_agent IS SUPPLIED.

      me->_o_agent = i_o_agent.

    ENDIF.

  ENDMETHOD.

  METHOD yif_aaic_func_call_tools~get_available_tools.

    FREE r_response.

    IF me->_o_agent IS NOT BOUND.

      r_response = 'Error! No Agent Instance available to retrieve the list of tools.'.

      RETURN.

    ENDIF.

    DATA(lt_tools) = me->_o_agent->get_tools( i_load_on_demand_tools = abap_true ).

    DATA(lo_aaic_util) = NEW ycl_aaic_util( ).

    r_response = lo_aaic_util->serialize(
      EXPORTING
        i_data       = lt_tools
    ).

  ENDMETHOD.

  METHOD yif_aaic_func_call_tools~request_tools.


  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

*    me->_o_agent = NEW ycl_aaic_agent(
*      i_agent_id = CONV #( '7EA3422BA1AC1FE0AF9AFAC0DF03AC74' )
*      i_chat_id  = CONV #( '' )
*    ).
*
*    out->write( me->get_available_tools( ) ).

  ENDMETHOD.

ENDCLASS.
