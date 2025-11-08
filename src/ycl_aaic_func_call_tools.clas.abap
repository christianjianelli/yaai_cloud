CLASS ycl_aaic_func_call_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_aaic_func_call_tools.

    ALIASES get_list FOR yif_aaic_func_call_tools~get_list.

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

  METHOD yif_aaic_func_call_tools~get_list.



  ENDMETHOD.

ENDCLASS.
