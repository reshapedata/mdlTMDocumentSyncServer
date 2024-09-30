#' 销售出库同步至数据中台
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
#' saleOut_sync_dmsServer()
saleOut_sync_dmsServer <- function(input,output,session,dms_token,wms_token) {

  txt_saleOut_fyear_wms = tsui::var_text('txt_saleOut_fyear_wms')

  txt_saleOut_fmonth_wms = tsui::var_text('txt_saleOut_fmonth_wms')
  shiny::observeEvent(input$btn_saleOut_sync_dms,{
    fyear =txt_saleOut_fyear_wms()
    fmonth=txt_saleOut_fmonth_wms()


    if(fyear==''||fmonth==''){
      tsui::pop_notice("请输入年份和月份")
    }
    else {
      data= mdlTMDocumentSyncPkg::TmWMS_saleOut_sekectBymonth(wms_token =wms_token ,fyear = fyear,fmonth = fmonth)
      data = as.data.frame(data)

      data = tsdo::na_standard(data)

      mdlTMDocumentSyncPkg::TmWMS_saleOut_deleteBymonth(dms_token = dms_token,fyear =fyear ,fmonth =fmonth )
      tsda::db_writeTable2(token = dms_token,table_name = 'rds_dms_src_t_sal_outstock',r_object = data,append = TRUE)
      tsui::pop_notice("上传完成")

    }





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
    tsui::pop_notice(txt_saleOut_fbillno_sync_erp())
    print("功能待完善")




  })


}




#' DMS销售出库按单据编号查询
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


  txt_saleOut_fbillno_view = tsui::var_text('txt_saleOut_fbillno_view')
  shiny::observeEvent(input$btn_saleOut_fbillno_view_dms,{

    fbillno=txt_saleOut_fbillno_view()

    data=mdlTMDocumentSyncPkg::Tmdms_saleOut_selectByfbillno(dms_token = dms_token,fbillno =fbillno)

    tsui::run_dataTable2(id = 'dt_saleOut',data = data)
    tsui::run_download_xlsx(id = 'dl_saleOut_fbillno_dms',data = data,filename = "销售出库按单查询数据.xlsx")




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
    fdate=date_tm_saleOut()
    data=mdlTMDocumentSyncPkg::Tmdms_saleOut_selectBydate(dms_token = dms_token,fdate =fdate )

    tsui::run_dataTable2(id = 'dt_saleOut',data = data)

  })


}

#' 销售出库按日期范围查询
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
#' saleOut_dateRange_view_dmsServer()
saleOut_dateRange_view_dmsServer <- function(input,output,session,dms_token) {

  date_tm_saleOut_FStartDate= tsui::var_date('date_tm_saleOut_FStartDate')

  date_tm_saleOut_FEndDate= tsui::var_date('date_tm_saleOut_FEndDate')

  shiny::observeEvent(input$btn_saleOut_dateRange_view_dms,{
    FStartDate=date_tm_saleOut_FStartDate()

    FEndDate=date_tm_saleOut_FEndDate()

    data=mdlTMDocumentSyncPkg::Tmdms_saleOut_selectBydateRange(dms_token =dms_token ,FStartDate =FStartDate ,FEndDate = FEndDate)


    tsui::run_dataTable2(id = 'dt_saleOut',data = data)

    tsui::run_download_xlsx(id = 'dl_saleOut_dateRange_dms',data = data,filename = "销售出库单数据.xlsx")





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
#' saleOutServer()
saleOutServer <- function(input,output,session,dms_token,wms_token) {


  mdlTMDocumentSyncServer::saleOut_sync_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token ,wms_token = wms_token)

  mdlTMDocumentSyncServer::saleOut_sync_erpServer(input =input ,output =output ,session = session,dms_token =dms_token )


  mdlTMDocumentSyncServer::saleOut_fbillno_view_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )

  mdlTMDocumentSyncServer::saleOut_date_view_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )

  mdlTMDocumentSyncServer::saleOut_dateRange_view_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )


}
