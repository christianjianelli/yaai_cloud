CLASS ycl_aaic_http_service_file_upl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES if_http_service_extension.

  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS: get_html RETURNING VALUE(ui_html) TYPE string.

ENDCLASS.



CLASS ycl_aaic_http_service_file_upl IMPLEMENTATION.

  METHOD if_http_service_extension~handle_request.

    DATA: lo_request TYPE REF TO if_web_http_request.

    CASE request->get_method(  ).

      WHEN CONV string( if_web_http_client=>get ).

        response->set_text( get_html( ) ).

      WHEN CONV string( if_web_http_client=>post ).

        TRY.

            lo_request = request->get_multipart( index = 1 ).

            DATA(l_file_content) = lo_request->get_text( ).

            DATA(l_filename) = lo_request->get_header_field( '~content_filename' ).

            NEW ycl_aaic_rag_db( )->create(
              EXPORTING
                i_filename = l_filename
                i_content  = l_file_content
              IMPORTING
                e_id       = DATA(l_id)
            ).

            response->set_text( |File { l_filename } uploaded successfully. Id: { l_id } | ).

          CATCH cx_web_message_error ##NO_HANDLER.

        ENDTRY.

    ENDCASE.

  ENDMETHOD.

  METHOD get_html.

    ui_html =
    |<!DOCTYPE HTML> \n| &&
     |<html> \n| &&
     |<head> \n| &&
     |    <meta http-equiv="X-UA-Compatible" content="IE=edge"> \n| &&
     |    <meta http-equiv='Content-Type' content='text/html;charset=UTF-8' /> \n| &&
     |    <title>ABAP AI tools Cloud - RAG File Uploader</title> \n| &&
     |    <script id="sap-ui-bootstrap" src="https://sapui5.hana.ondemand.com/resources/sap-ui-core.js" \n| &&
     |        data-sap-ui-theme="sap_horizon" data-sap-ui-xx-bindingSyntax="complex" data-sap-ui-compatVersion="edge" \n| &&
     |        data-sap-ui-async="true"> \n| &&
     |    </script> \n| &&
     |    <script> \n| &&
     |        sap.ui.require(['sap/ui/core/Core'], (oCore, ) => \{ \n| &&
     | \n| &&
     |            sap.ui.getCore().loadLibrary("sap.f", \{ \n| &&
     |                async: true \n| &&
     |            \}).then(() => \{ \n| &&
     |                let shell = new sap.f.ShellBar("shell") \n| &&
     |                shell.setTitle("ABAP AI tools Cloud - RAG File Uploader") \n| &&
     |                shell.placeAt("uiArea") \n| &&
     |                sap.ui.getCore().loadLibrary("sap.ui.layout", \{ \n| &&
     |                    async: true \n| &&
     |                \}).then(() => \{ \n| &&
     |                    let layout = new sap.ui.layout.VerticalLayout("layout") \n| &&
     |                    layout.placeAt("uiArea") \n| &&
     |                    let line2 = new sap.ui.layout.HorizontalLayout("line2") \n| &&
     |                    let line3 = new sap.ui.layout.HorizontalLayout("line3") \n| &&
     |                    let line4 = new sap.ui.layout.HorizontalLayout("line4") \n| &&
     |                    sap.ui.getCore().loadLibrary("sap.m", \{ \n| &&
     |                        async: true \n| &&
     |                    \}).then(() => \{\}) \n| &&
     |                    let button = new sap.m.Button("button") \n| &&
     |                    button.setText("Upload File") \n| &&
     |                    button.attachPress(function () \{ \n| &&
     |                        let oFileUploader = oCore.byId("fileToUpload") \n| &&
     |                        if (!oFileUploader.getValue()) \{ \n| &&
     |                            sap.m.MessageToast.show("Choose a file first") \n| &&
     |                            return \n| &&
     |                        \} \n| &&
     |                       oFileUploader.upload() \n| &&
     |                    \}) \n| &&
     |                    line2.placeAt("layout") \n| &&
     |                    line3.placeAt("layout") \n| &&
     |                    line4.placeAt("layout") \n| &&
     |                    sap.ui.getCore().loadLibrary("sap.ui.unified", \{ \n| &&
     |                        async: true \n| &&
     |                    \}).then(() => \{ \n| &&
     |                        var fileUploader = new sap.ui.unified.FileUploader( \n| &&
     |                            "fileToUpload") \n| &&
     |                        fileUploader.setFileType("md") \n| &&
     |                        fileUploader.setWidth("400px") \n| &&
     |                        fileUploader.placeAt("line2") \n| &&
     |                        button.placeAt("line2") \n| &&
     |                        fileUploader.setPlaceholder( \n| &&
     |                            "Choose File for Upload...") \n| &&
     |                        fileUploader.attachUploadComplete(function (oEvent) \{ \n| &&
     |                           alert(oEvent.getParameters().response)  \n| &&
     |                       \})   \n| &&
     | \n| &&
     |                    \}) \n| &&
     |                \}) \n| &&
     |            \}) \n| &&
     |        \}) \n| &&
     |    </script> \n| &&
     |</head> \n| &&
     |<body class="sapUiBody"> \n| &&
     |    <div id="uiArea"></div> \n| &&
     |</body> \n| &&
     | \n| &&
     |</html> |.
  ENDMETHOD.

ENDCLASS.


