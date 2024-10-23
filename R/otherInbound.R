#' 其他入库同步至数据中台
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
#' otherInbound_sync_dmsServer()
otherInbound_sync_dmsServer <- function(input,output,session,dms_token,wms_token) {

  txt_otherInbound_fyear_wms = tsui::var_text('txt_otherInbound_fyear_wms')

  txt_otherInbound_fmonth_wms = tsui::var_text('txt_otherInbound_fmonth_wms')
  shiny::observeEvent(input$btn_otherInbound_sync_dms,{
    fyear =txt_otherInbound_fyear_wms()
    fmonth=txt_otherInbound_fmonth_wms()


    if(fyear==''||fmonth==''){
      tsui::pop_notice("请输入年份和月份")
    }
    else {
      data= mdlTMDocumentSyncPkg::TmWMS_otherInbound_sekectBymonth(wms_token =wms_token ,fyear = fyear,fmonth = fmonth)
      data = as.data.frame(data)

      data = tsdo::na_standard(data)

      mdlTMDocumentSyncPkg::TmWMS_otherInbound_deleteBymonth(dms_token = dms_token,fyear =fyear ,fmonth =fmonth )
      tsda::db_writeTable2(token = dms_token,table_name = 'rds_dms_src_t_sal_returnstock',r_object = data,append = TRUE)
      tsui::pop_notice("上传完成")

    }





  })


}


#' 其他入库同步至ERP
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
#' otherInbound_sync_erpServer()
otherInbound_sync_erpServer <- function(input,output,session,dms_token) {

  txt_otherInbound_fbillno_sync_erp = tsui::var_text('txt_otherInbound_fbillno_sync_erp')
  shiny::observeEvent(input$btn_otherInbound_sync_erp,{
    tsui::pop_notice(txt_otherInbound_fbillno_sync_erp())
    print("功能待完善")




  })


}




#' DMS其他入库按单据编号查询
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
#' otherInbound_fbillno_view_dmsServer()
otherInbound_fbillno_view_dmsServer <- function(input,output,session,dms_token) {


  txt_otherInbound_fbillno_view = tsui::var_text('txt_otherInbound_fbillno_view')
  shiny::observeEvent(input$btn_otherInbound_fbillno_view_dms,{

    fbillno=txt_otherInbound_fbillno_view()

    data=mdlTMDocumentSyncPkg::Tmdms_otherInbound_selectByfbillno(dms_token = dms_token,fbillno =fbillno)

    tsui::run_dataTable2(id = 'dt_otherInbound',data = data)
    tsui::run_download_xlsx(id = 'dl_otherInbound_fbillno_dms',data = data,filename = "其他入库按单查询数据.xlsx")




  })


}

#' 其他入库按日期查询
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
#' otherInbound_date_view_dmsServer()
otherInbound_date_view_dmsServer <- function(input,output,session,dms_token) {

  date_tm_otherInbound= tsui::var_date('date_tm_otherInbound')

  shiny::observeEvent(input$btn_otherInbound_date_view_dms,{
    fdate=date_tm_otherInbound()
    data=mdlTMDocumentSyncPkg::Tmdms_otherInbound_selectBydate(dms_token = dms_token,fdate =fdate )

    tsui::run_dataTable2(id = 'dt_otherInbound',data = data)

  })


}

#' 其他入库按日期范围查询
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
#' otherInbound_dateRange_view_dmsServer()
otherInbound_dateRange_view_dmsServer <- function(input,output,session,dms_token) {

  date_tm_otherInbound_FStartDate= tsui::var_date('date_tm_otherInbound_FStartDate')

  date_tm_otherInbound_FEndDate= tsui::var_date('date_tm_otherInbound_FEndDate')

  shiny::observeEvent(input$btn_otherInbound_dateRange_view_dms,{
    FStartDate=date_tm_otherInbound_FStartDate()

    FEndDate=date_tm_otherInbound_FEndDate()

    data=mdlTMDocumentSyncPkg::Tmdms_otherInbound_selectBydateRange(dms_token =dms_token ,FStartDate =FStartDate ,FEndDate = FEndDate)


    tsui::run_dataTable2(id = 'dt_otherInbound',data = data)

    tsui::run_download_xlsx(id = 'dl_otherInbound_dateRange_dms',data = data,filename = "其他入库单数据.xlsx")





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
#' otherInboundServer()
otherInboundServer <- function(input,output,session,dms_token,wms_token) {


  mdlTMDocumentSyncServer::otherInbound_sync_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token ,wms_token = wms_token)

  mdlTMDocumentSyncServer::otherInbound_sync_erpServer(input =input ,output =output ,session = session,dms_token =dms_token )


  mdlTMDocumentSyncServer::otherInbound_fbillno_view_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )

  mdlTMDocumentSyncServer::otherInbound_date_view_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )

  mdlTMDocumentSyncServer::otherInbound_dateRange_view_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )


}
