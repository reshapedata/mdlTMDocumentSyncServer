#' 采购入库同步
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
#' purchaseInbound_sync_dmsServer()
purchaseInbound_sync_dmsServer <- function(input,output,session,dms_token) {


  var_txt_purchaseInbound_fbillno_sync = tsui::var_text('txt_purchaseInbound_fbillno_sync')
  shiny::observeEvent(input$btn_purchaseInbound_sync_dms,{




  })


}




#' 采购入库按单据编号查询
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
#' purchaseInbound_fbillno_view_dmsServer()
purchaseInbound_fbillno_view_dmsServer <- function(input,output,session,dms_token) {


  var_txt_purchaseInbound_fbillno_view = tsui::var_text('txt_purchaseInbound_fbillno_view')
  shiny::observeEvent(input$btn_purchaseInbound_fbillno_view_dms,{




  })


}

#' 采购入库按日期查询
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
#' purchaseInbound_date_view_dmsServer()
purchaseInbound_date_view_dmsServer <- function(input,output,session,dms_token) {


  var_txt_purchaseInbound_fyear_view = tsui::var_text('txt_purchaseInbound_fyear_view')

  var_txt_purchaseInbound_fmonth_view = tsui::var_text('txt_purchaseInbound_fmonth_view')
  shiny::observeEvent(input$btn_purchaseInbound_date_view_dms,{








  })


}

#' 采购入库功能汇总
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
#' purchaseInboundServer()
purchaseInboundServer <- function(input,output,session,dms_token) {


  mdlTMDocumentSyncServer::purchaseInbound_sync_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )

  mdlTMDocumentSyncServer::purchaseInbound_fbillno_view_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )

  mdlTMDocumentSyncServer::purchaseInbound_date_view_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )



}
