#' 销售退货同步至数据中台
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
#' saleReturn_sync_dmsServer()
saleReturn_sync_dmsServer <- function(input,output,session,dms_token,wms_token) {

  txt_saleReturn_fyear_wms = tsui::var_text('txt_saleReturn_fyear_wms')

  txt_saleReturn_fmonth_wms = tsui::var_text('txt_saleReturn_fmonth_wms')
  shiny::observeEvent(input$btn_saleReturn_sync_dms,{
    fyear =txt_saleReturn_fyear_wms()
    fmonth=txt_saleReturn_fmonth_wms()


    if(fyear==''||fmonth==''){
      tsui::pop_notice("请输入年份和月份")
    }
    else {
      data= mdlTMDocumentSyncPkg::TmWMS_saleReturn_sekectBymonth(wms_token =wms_token ,fyear = fyear,fmonth = fmonth)
      data = as.data.frame(data)

      data = tsdo::na_standard(data)

      mdlTMDocumentSyncPkg::TmWMS_saleReturn_deleteBymonth(dms_token = dms_token,fyear =fyear ,fmonth =fmonth )
      tsda::db_writeTable2(token = dms_token,table_name = 'rds_dms_src_t_sal_returnstock',r_object = data,append = TRUE)
      tsui::pop_notice("上传完成")

    }





  })


}


#' 销售退货同步至ERP
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
#' saleReturn_sync_erpServer()
saleReturn_sync_erpServer <- function(input,output,session,dms_token) {

  txt_saleReturn_fbillno_sync_erp = tsui::var_text('txt_saleReturn_fbillno_sync_erp')
  shiny::observeEvent(input$btn_saleReturn_sync_erp,{
    tsui::pop_notice(txt_saleReturn_fbillno_sync_erp())
    print("功能待完善")




  })


}




#' DMS销售退货按单据编号查询
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


  txt_saleReturn_fbillno_view = tsui::var_text('txt_saleReturn_fbillno_view')
  shiny::observeEvent(input$btn_saleReturn_fbillno_view_dms,{

    fbillno=txt_saleReturn_fbillno_view()

    data=mdlTMDocumentSyncPkg::Tmdms_saleReturn_selectByfbillno(dms_token = dms_token,fbillno =fbillno)

    tsui::run_dataTable2(id = 'dt_saleReturn',data = data)
    tsui::run_download_xlsx(id = 'dl_saleReturn_fbillno_dms',data = data,filename = "销售退货按单查询数据.xlsx")




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

  date_tm_saleReturn= tsui::var_date('date_tm_saleReturn')

  shiny::observeEvent(input$btn_saleReturn_date_view_dms,{
    fdate=date_tm_saleReturn()
    data=mdlTMDocumentSyncPkg::Tmdms_saleReturn_selectBydate(dms_token = dms_token,fdate =fdate )

    tsui::run_dataTable2(id = 'dt_saleReturn',data = data)

  })


}

#' 销售退货按日期范围查询
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
#' saleReturn_dateRange_view_dmsServer()
saleReturn_dateRange_view_dmsServer <- function(input,output,session,dms_token) {

  date_tm_saleReturn_FStartDate= tsui::var_date('date_tm_saleReturn_FStartDate')

  date_tm_saleReturn_FEndDate= tsui::var_date('date_tm_saleReturn_FEndDate')

  shiny::observeEvent(input$btn_saleReturn_dateRange_view_dms,{
    FStartDate=date_tm_saleReturn_FStartDate()

    FEndDate=date_tm_saleReturn_FEndDate()

    data=mdlTMDocumentSyncPkg::Tmdms_saleReturn_selectBydateRange(dms_token =dms_token ,FStartDate =FStartDate ,FEndDate = FEndDate)


    tsui::run_dataTable2(id = 'dt_saleReturn',data = data)

    tsui::run_download_xlsx(id = 'dl_saleReturn_dateRange_dms',data = data,filename = "销售退货单数据.xlsx")





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
#' saleReturnServer()
saleReturnServer <- function(input,output,session,dms_token,wms_token) {


  mdlTMDocumentSyncServer::saleReturn_sync_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token ,wms_token = wms_token)

  mdlTMDocumentSyncServer::saleReturn_sync_erpServer(input =input ,output =output ,session = session,dms_token =dms_token )


  mdlTMDocumentSyncServer::saleReturn_fbillno_view_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )

  mdlTMDocumentSyncServer::saleReturn_date_view_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )

  mdlTMDocumentSyncServer::saleReturn_dateRange_view_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )


}
