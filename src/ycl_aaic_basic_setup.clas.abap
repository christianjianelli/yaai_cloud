CLASS ycl_aaic_basic_setup DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_aaic_basic_setup IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    INSERT yaaic_api FROM TABLE @( VALUE #( ( id = 'OPENAI' base_url = 'https://api.openai.com' )
                                            ( id = 'ANTHROPIC' base_url = 'https://api.anthropic.com' )
                                            ( id = 'GOOGLE' base_url = 'https://generativelanguage.googleapis.com' )
                                            ( id = 'MISTRAL' base_url = 'https://api.mistral.ai' ) ) ) ACCEPTING DUPLICATE KEYS.

    INSERT yaaic_model FROM TABLE @( VALUE #( ( id = 'OPENAI' model = 'gpt-5-nano' default_model = abap_true )
                                              ( id = 'OPENAI' model = 'gpt-5' default_model = abap_false )
                                              ( id = 'ANTHROPIC' model = 'claude-sonnet-4-20250514' default_model = abap_true )
                                              ( id = 'GOOGLE' model = 'gemini-2.5-flash' default_model = abap_true )
                                              ( id = 'MISTRAL' model = 'mistral-medium-latest' default_model = abap_true )
                                              ( id = 'MISTRAL' model = 'mistral-large-latest' default_model = abap_false ) ) ) ACCEPTING DUPLICATE KEYS.

    INSERT yaaic_tool FROM TABLE @( VALUE #( ( class_name = 'ycl_aaic_func_call_tools'
                                               method_name = 'get_available_tools'
                                               description = 'Use this tool to retrieve the list of tools available for you to use.' )

                                             ( class_name = 'ycl_aaic_func_call_tools'
                                               method_name = 'request_tools'
                                               description = 'Use this tool to request the schema of the tools you want to use.' )

                                             ( class_name = 'ycl_aaic_rag_tools'
                                               method_name = 'get_list_of_documents'
                                               description = 'Use this tool to retrieve the list of documents available for you.' )

                                             ( class_name = 'ycl_aaic_rag_tools'
                                               method_name = 'get_documentation'
                                               description = 'Use this tool to retrieve the content of the documentation.' )

                                             ( class_name = 'ycl_aaic_rag_tools'
                                               method_name = 'create_documentation'
                                               description = 'Use this tool to create a documentation.' )

                                             ( class_name = 'ycl_aaic_rag_tools'
                                               method_name = 'update_documentation'
                                               description = 'Use this tool to create a documentation.' ) ) ) ACCEPTING DUPLICATE KEYS.

  ENDMETHOD.
ENDCLASS.
