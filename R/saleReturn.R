#' 销售退货同步
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
#' saleReturn_sync_dmsServer()
saleReturn_sync_dmsServer <- function(input,output,session,dms_token) {


  var_txt_saleReturn_fbillno_sync = tsui::var_text('txt_saleReturn_fbillno_sync')
  shiny::observeEvent(input$btn_saleReturn_sync_dms,{




  })


}




#' 销售退货按单据编号查询
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
#' saleReturn_fbillno_view_dmsServer()
saleReturn_fbillno_view_dmsServer <- function(input,output,session,dms_token) {


  var_txt_saleReturn_fbillno_view = tsui::var_text('txt_saleReturn_fbillno_view')
  shiny::observeEvent(input$btn_saleReturn_fbillno_view_dms,{




  })


}

#' 销售退货按日期查询
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
#' saleReturn_date_view_dmsServer()
saleReturn_date_view_dmsServer <- function(input,output,session,dms_token) {


  var_txt_saleReturn_fyear_view = tsui::var_text('txt_saleReturn_fyear_view')

  var_txt_saleReturn_fmonth_view = tsui::var_text('txt_saleReturn_fmonth_view')
  shiny::observeEvent(input$btn_saleReturn_date_view_dms,{








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
#' saleReturnServer()
saleReturnServer <- function(input,output,session,dms_token) {


  mdlTMDocumentSyncServer::saleReturn_sync_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )

  mdlTMDocumentSyncServer::saleReturn_fbillno_view_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )

  mdlTMDocumentSyncServer::saleReturn_date_view_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )



}
