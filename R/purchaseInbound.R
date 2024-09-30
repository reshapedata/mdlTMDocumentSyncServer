#' 采购入库同步至数据中台
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#' @param wms_token
#'
#' @return 返回值
#' @export
#'
#' @examples
#' purchaseInbound_sync_dmsServer()
purchaseInbound_sync_dmsServer <- function(input,output,session,dms_token,wms_token) {

  txt_purchaseInbound_fyear_wms = tsui::var_text('txt_purchaseInbound_fyear_wms')

  txt_purchaseInbound_fmonth_wms = tsui::var_text('txt_purchaseInbound_fmonth_wms')
  shiny::observeEvent(input$btn_purchaseInbound_sync_dms,{
    fyear =txt_purchaseInbound_fyear_wms()
    fmonth=txt_purchaseInbound_fmonth_wms()


    if(fyear==''||fmonth==''){
      tsui::pop_notice("请输入年份和月份")
    }
    else {
      data= mdlTMDocumentSyncPkg::TmWMS_purchaseInbound_sekectBymonth(wms_token =wms_token ,fyear = fyear,fmonth = fmonth)
      data = as.data.frame(data)

      data = tsdo::na_standard(data)

      mdlTMDocumentSyncPkg::TmWMS_purchaseInbound_deleteBymonth(dms_token = dms_token,fyear =fyear ,fmonth =fmonth )
      tsda::db_writeTable2(token = dms_token,table_name = 'rds_dms_src_t_pur_instock',r_object = data,append = TRUE)
      tsui::pop_notice("上传完成")

    }





  })


}


#' 采购入库同步至ERP
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
#' purchaseInbound_sync_erpServer()
purchaseInbound_sync_erpServer <- function(input,output,session,dms_token) {

  txt_purchaseInbound_fbillno_sync_erp = tsui::var_text('txt_purchaseInbound_fbillno_sync_erp')
  shiny::observeEvent(input$btn_purchaseInbound_sync_erp,{
    tsui::pop_notice(txt_purchaseInbound_fbillno_sync_erp())
    print("功能待完善")




  })


}




#' DMS采购入库按单据编号查询
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


  txt_purchaseInbound_fbillno_view = tsui::var_text('txt_purchaseInbound_fbillno_view')
  shiny::observeEvent(input$btn_purchaseInbound_fbillno_view_dms,{

    fbillno=txt_purchaseInbound_fbillno_view()

    data=mdlTMDocumentSyncPkg::Tmdms_purchaseInbound_selectByfbillno(dms_token = dms_token,fbillno =fbillno)

    tsui::run_dataTable2(id = 'dt_purchaseInbound',data = data)
    tsui::run_download_xlsx(id = 'dl_purchaseInbound_fbillno_dms',data = data,filename = "采购入库按单查询数据.xlsx")




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

  date_tm_purchaseInbound= tsui::var_date('date_tm_purchaseInbound')

  shiny::observeEvent(input$btn_purchaseInbound_date_view_dms,{
    fdate=date_tm_purchaseInbound()
    data=mdlTMDocumentSyncPkg::Tmdms_purchaseInbound_selectBydate(dms_token = dms_token,fdate =fdate )

    tsui::run_dataTable2(id = 'dt_purchaseInbound',data = data)

  })


}

#' 采购入库按日期范围查询
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
#' purchaseInbound_dateRange_view_dmsServer()
purchaseInbound_dateRange_view_dmsServer <- function(input,output,session,dms_token) {

  date_tm_purchaseInbound_FStartDate= tsui::var_date('date_tm_purchaseInbound_FStartDate')

  date_tm_purchaseInbound_FEndDate= tsui::var_date('date_tm_purchaseInbound_FEndDate')

  shiny::observeEvent(input$btn_purchaseInbound_dateRange_view_dms,{
    FStartDate=date_tm_purchaseInbound_FStartDate()

    FEndDate=date_tm_purchaseInbound_FEndDate()

    data=mdlTMDocumentSyncPkg::Tmdms_purchaseInbound_selectBydateRange(dms_token =dms_token ,FStartDate =FStartDate ,FEndDate = FEndDate)


    tsui::run_dataTable2(id = 'dt_purchaseInbound',data = data)

    tsui::run_download_xlsx(id = 'dl_purchaseInbound_dateRange_dms',data = data,filename = "采购入库单数据.xlsx")





  })


}


#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#' @param wms_token
#' @return 返回值
#' @export
#'
#' @examples
#' purchaseInboundServer()
purchaseInboundServer <- function(input,output,session,dms_token,wms_token) {


  mdlTMDocumentSyncServer::purchaseInbound_sync_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token ,wms_token = wms_token)

  mdlTMDocumentSyncServer::purchaseInbound_sync_erpServer(input =input ,output =output ,session = session,dms_token =dms_token )


  mdlTMDocumentSyncServer::purchaseInbound_fbillno_view_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )

  mdlTMDocumentSyncServer::purchaseInbound_date_view_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )

  mdlTMDocumentSyncServer::purchaseInbound_dateRange_view_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )


}
