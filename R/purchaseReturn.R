#' 采购退料同步
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' purchaseReturn_sync_dmsServer()
purchaseReturn_sync_dmsServer <- function(input,output,session,dms_token) {


  var_txt_purchaseReturn_fbillno_sync = tsui::var_text('txt_purchaseReturn_fbillno_sync')
  shiny::observeEvent(input$btn_purchaseReturn_sync_dms,{




  })


}




#' 采购退料按单据编号查询
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' purchaseReturn_fbillno_view_dmsServer()
purchaseReturn_fbillno_view_dmsServer <- function(input,output,session,dms_token) {


  var_txt_purchaseReturn_fbillno_view = tsui::var_text('txt_purchaseReturn_fbillno_view')
  shiny::observeEvent(input$btn_purchaseReturn_fbillno_view_dms,{




  })


}

#' 采购退料按日期查询
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' purchaseReturn_date_view_dmsServer()
purchaseReturn_date_view_dmsServer <- function(input,output,session,dms_token) {


  var_txt_purchaseReturn_fyear_view = tsui::var_text('txt_purchaseReturn_fyear_view')

  var_txt_purchaseReturn_fmonth_view = tsui::var_text('txt_purchaseReturn_fmonth_view')
  shiny::observeEvent(input$btn_purchaseReturn_date_view_dms,{








  })


}

#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' purchaseReturnServer()
purchaseReturnServer <- function(input,output,session,dms_token) {


  mdlTMDocumentSyncServer::purchaseReturn_sync_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )

  mdlTMDocumentSyncServer::purchaseReturn_fbillno_view_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )

  mdlTMDocumentSyncServer::purchaseReturn_date_view_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )



}
