#' 销售出库同步至数据中台
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
#' saleOut_sync_dmsServer()
saleOut_sync_dmsServer <- function(input,output,session,dms_token) {

  txt_saleOut_fyear_wms = tsui::var_text('txt_saleOut_fyear_wms')

  txt_saleOut_fmonth_wms = tsui::var_text('txt_saleOut_fmonth_wms')
  shiny::observeEvent(input$btn_saleOut_sync_dms,{




  })


}


#' 销售出库同步至ERP
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
#' saleOut_sync_erpServer()
saleOut_sync_erpServer <- function(input,output,session,dms_token) {

  txt_saleOut_fbillno_sync_erp = tsui::var_text('txt_saleOut_fbillno_sync_erp')
  shiny::observeEvent(input$btn_saleOut_sync_erp,{




  })


}




#' 销售出库按单据编号查询
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
#' saleOut_fbillno_view_dmsServer()
saleOut_fbillno_view_dmsServer <- function(input,output,session,dms_token) {


  var_txt_saleOut_fbillno_view = tsui::var_text('txt_saleOut_fbillno_view')
  shiny::observeEvent(input$btn_saleOut_fbillno_view_dms,{




  })


}

#' 销售出库按日期查询
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
#' saleOut_date_view_dmsServer()
saleOut_date_view_dmsServer <- function(input,output,session,dms_token) {

  date_tm_saleOut= tsui::var_date('date_tm_saleOut')

  shiny::observeEvent(input$btn_saleOut_date_view_dms,{








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
#' saleOutServer()
saleOutServer <- function(input,output,session,dms_token) {


  mdlTMDocumentSyncServer::saleOut_sync_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )

  mdlTMDocumentSyncServer::saleOut_sync_erpServer(input =input ,output =output ,session = session,dms_token =dms_token )


  mdlTMDocumentSyncServer::saleOut_fbillno_view_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )

  mdlTMDocumentSyncServer::saleOut_date_view_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )



}
