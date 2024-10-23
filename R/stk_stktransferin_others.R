#' 直接调拨-其他同步至数据中台
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
#' stk_stktransferin_others_sync_dmsServer()
stk_stktransferin_others_sync_dmsServer <- function(input,output,session,dms_token,wms_token) {

  txt_stk_stktransferin_others_fyear_wms = tsui::var_text('txt_stk_stktransferin_others_fyear_wms')

  txt_stk_stktransferin_others_fmonth_wms = tsui::var_text('txt_stk_stktransferin_others_fmonth_wms')
  shiny::observeEvent(input$btn_stk_stktransferin_others_sync_dms,{
    fyear =txt_stk_stktransferin_others_fyear_wms()
    fmonth=txt_stk_stktransferin_others_fmonth_wms()


    if(fyear==''||fmonth==''){
      tsui::pop_notice("请输入年份和月份")
    }
    else {
      data= mdlTMDocumentSyncPkg::TmWMS_stk_stktransferin_others_sekectBymonth(wms_token =wms_token ,fyear = fyear,fmonth = fmonth)
      data = as.data.frame(data)

      data = tsdo::na_standard(data)

      mdlTMDocumentSyncPkg::TmWMS_stk_stktransferin_others_deleteBymonth(dms_token = dms_token,fyear =fyear ,fmonth =fmonth )
      tsda::db_writeTable2(token = dms_token,table_name = 'rds_dms_src_t_stk_stktransferin_others',r_object = data,append = TRUE)
      tsui::pop_notice("上传完成")

    }





  })


}


#' 直接调拨-其他同步至ERP
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
#' stk_stktransferin_others_sync_erpServer()
stk_stktransferin_others_sync_erpServer <- function(input,output,session,dms_token) {

  txt_stk_stktransferin_others_fbillno_sync_erp = tsui::var_text('txt_stk_stktransferin_others_fbillno_sync_erp')
  shiny::observeEvent(input$btn_stk_stktransferin_others_sync_erp,{
    tsui::pop_notice(txt_stk_stktransferin_others_fbillno_sync_erp())
    print("功能待完善")




  })


}




#' DMS直接调拨-其他按单据编号查询
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
#' stk_stktransferin_others_fbillno_view_dmsServer()
stk_stktransferin_others_fbillno_view_dmsServer <- function(input,output,session,dms_token) {


  txt_stk_stktransferin_others_fbillno_view = tsui::var_text('txt_stk_stktransferin_others_fbillno_view')
  shiny::observeEvent(input$btn_stk_stktransferin_others_fbillno_view_dms,{

    fbillno=txt_stk_stktransferin_others_fbillno_view()

    data=mdlTMDocumentSyncPkg::Tmdms_stk_stktransferin_others_selectByfbillno(dms_token = dms_token,fbillno =fbillno)

    tsui::run_dataTable2(id = 'dt_stk_stktransferin_others',data = data)
    tsui::run_download_xlsx(id = 'dl_stk_stktransferin_others_fbillno_dms',data = data,filename = "直接调拨-其他按单查询数据.xlsx")




  })


}

#' 直接调拨-其他按日期查询
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
#' stk_stktransferin_others_date_view_dmsServer()
stk_stktransferin_others_date_view_dmsServer <- function(input,output,session,dms_token) {

  date_tm_stk_stktransferin_others= tsui::var_date('date_tm_stk_stktransferin_others')

  shiny::observeEvent(input$btn_stk_stktransferin_others_date_view_dms,{
    fdate=date_tm_stk_stktransferin_others()
    data=mdlTMDocumentSyncPkg::Tmdms_stk_stktransferin_others_selectBydate(dms_token = dms_token,fdate =fdate )

    tsui::run_dataTable2(id = 'dt_stk_stktransferin_others',data = data)

  })


}

#' 直接调拨-其他按日期范围查询
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
#' stk_stktransferin_others_dateRange_view_dmsServer()
stk_stktransferin_others_dateRange_view_dmsServer <- function(input,output,session,dms_token) {

  date_tm_stk_stktransferin_others_FStartDate= tsui::var_date('date_tm_stk_stktransferin_others_FStartDate')

  date_tm_stk_stktransferin_others_FEndDate= tsui::var_date('date_tm_stk_stktransferin_others_FEndDate')

  shiny::observeEvent(input$btn_stk_stktransferin_others_dateRange_view_dms,{
    FStartDate=date_tm_stk_stktransferin_others_FStartDate()

    FEndDate=date_tm_stk_stktransferin_others_FEndDate()

    data=mdlTMDocumentSyncPkg::Tmdms_stk_stktransferin_others_selectBydateRange(dms_token =dms_token ,FStartDate =FStartDate ,FEndDate = FEndDate)


    tsui::run_dataTable2(id = 'dt_stk_stktransferin_others',data = data)

    tsui::run_download_xlsx(id = 'dl_stk_stktransferin_others_dateRange_dms',data = data,filename = "直接调拨-其他数据.xlsx")





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
#' stk_stktransferin_othersServer()
stk_stktransferin_othersServer <- function(input,output,session,dms_token,wms_token) {


  mdlTMDocumentSyncServer::stk_stktransferin_others_sync_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token ,wms_token = wms_token)

  mdlTMDocumentSyncServer::stk_stktransferin_others_sync_erpServer(input =input ,output =output ,session = session,dms_token =dms_token )


  mdlTMDocumentSyncServer::stk_stktransferin_others_fbillno_view_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )

  mdlTMDocumentSyncServer::stk_stktransferin_others_date_view_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )

  mdlTMDocumentSyncServer::stk_stktransferin_others_dateRange_view_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )


}
