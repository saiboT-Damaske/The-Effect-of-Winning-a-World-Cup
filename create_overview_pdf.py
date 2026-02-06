"""
Script to generate a PDF overview of the Master Thesis project:
"The Effect of Winning a World Cup"
"""

from reportlab.lib.pagesizes import A4
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.lib.units import cm, mm
from reportlab.lib.colors import HexColor, black, white
from reportlab.platypus import (
    SimpleDocTemplate, Paragraph, Spacer, Table, TableStyle, 
    PageBreak, Image, ListFlowable, ListItem
)
from reportlab.lib.enums import TA_CENTER, TA_LEFT, TA_JUSTIFY
import os

# Define colors
PRIMARY_COLOR = HexColor('#1a365d')  # Dark blue
ACCENT_COLOR = HexColor('#2c5282')   # Medium blue
LIGHT_BLUE = HexColor('#bee3f8')     # Light blue for table headers
TEXT_COLOR = HexColor('#2d3748')     # Dark gray for text

def create_styles():
    """Create custom paragraph styles."""
    styles = getSampleStyleSheet()
    
    # Title style
    styles.add(ParagraphStyle(
        name='MainTitle',
        parent=styles['Heading1'],
        fontSize=24,
        textColor=PRIMARY_COLOR,
        alignment=TA_CENTER,
        spaceAfter=20,
        spaceBefore=30,
        fontName='Helvetica-Bold'
    ))
    
    # Subtitle style
    styles.add(ParagraphStyle(
        name='Subtitle',
        parent=styles['Normal'],
        fontSize=14,
        textColor=ACCENT_COLOR,
        alignment=TA_CENTER,
        spaceAfter=40,
        fontName='Helvetica'
    ))
    
    # Section header
    styles.add(ParagraphStyle(
        name='SectionHeader',
        parent=styles['Heading2'],
        fontSize=16,
        textColor=PRIMARY_COLOR,
        spaceBefore=20,
        spaceAfter=10,
        fontName='Helvetica-Bold',
        borderPadding=(0, 0, 5, 0),
        borderWidth=0,
        borderColor=ACCENT_COLOR
    ))
    
    # Body text
    styles.add(ParagraphStyle(
        name='CustomBody',
        parent=styles['Normal'],
        fontSize=11,
        textColor=TEXT_COLOR,
        alignment=TA_JUSTIFY,
        spaceAfter=10,
        leading=16,
        fontName='Helvetica'
    ))
    
    # Bullet list item
    styles.add(ParagraphStyle(
        name='BulletItem',
        parent=styles['Normal'],
        fontSize=11,
        textColor=TEXT_COLOR,
        leftIndent=20,
        spaceAfter=5,
        fontName='Helvetica'
    ))
    
    return styles

def create_overview_pdf():
    """Generate the thesis overview PDF."""
    
    output_path = 'thesis_overview.pdf'
    doc = SimpleDocTemplate(
        output_path,
        pagesize=A4,
        rightMargin=2*cm,
        leftMargin=2*cm,
        topMargin=2*cm,
        bottomMargin=2*cm
    )
    
    styles = create_styles()
    story = []
    
    # === TITLE PAGE ===
    story.append(Spacer(1, 3*cm))
    story.append(Paragraph("The Effect of Winning a World Cup", styles['MainTitle']))
    story.append(Paragraph("LMU Master Thesis - Project Overview", styles['Subtitle']))
    story.append(Spacer(1, 1*cm))
    
    story.append(Paragraph("Tobias Damaske", styles['Subtitle']))
    story.append(Spacer(1, 2*cm))
    
    # Brief abstract
    abstract_text = """
    This thesis investigates the macroeconomic effects of winning the FIFA World Cup on 
    national economies. Building upon and replicating the methodology of Mello (2024) 
    "A Kick for GDP", the analysis examines whether World Cup victories generate measurable 
    impacts on GDP growth, consumption, investment, and trade for winning nations.
    """
    story.append(Paragraph(abstract_text.strip(), styles['CustomBody']))
    
    story.append(PageBreak())
    
    # === RESEARCH OVERVIEW ===
    story.append(Paragraph("1. Research Overview", styles['SectionHeader']))
    
    overview_text = """
    This project replicates and extends the empirical analysis from Mello's "A Kick for GDP" 
    paper published in Oxford Bulletin of Economics and Statistics (2024). The study uses 
    quarterly macroeconomic data from OECD countries spanning 1961–2021 to assess whether 
    winning the FIFA World Cup produces identifiable economic effects.
    """
    story.append(Paragraph(overview_text.strip(), styles['CustomBody']))
    
    # Key research questions
    story.append(Paragraph("<b>Key Research Questions:</b>", styles['CustomBody']))
    questions = [
        "Does winning the World Cup affect GDP growth in the winning country?",
        "Are there effects on consumption, investment, exports, and imports?",
        "How do effects differ between World Cup hosts and winners?",
        "Are the effects temporary or persistent?",
    ]
    for q in questions:
        story.append(Paragraph(f"• {q}", styles['BulletItem']))
    
    story.append(Spacer(1, 0.5*cm))
    
    # === DATA SECTION ===
    story.append(Paragraph("2. Data", styles['SectionHeader']))
    
    data_text = """
    The analysis uses quarterly national accounts data from the OECD QNA database, specifically 
    chain-linked volume estimates in PPP-adjusted 2015 US dollars (VPVOBARSA specification). 
    The dataset includes over 8,700 country-quarter observations.
    """
    story.append(Paragraph(data_text.strip(), styles['CustomBody']))
    
    # Data specifications table
    data_specs = [
        ['Specification', 'Value'],
        ['Time Period', '1961-Q1 to 2021-Q4'],
        ['Total Observations', '8,737'],
        ['Number of Countries', '48'],
        ['Winner Countries', '6 (BRA, DEU, ESP, FRA, GBR, ITA)'],
        ['Control Countries', '42'],
        ['Frequency', 'Quarterly, Seasonally Adjusted'],
        ['Unit', '2015 PPP USD (Chain-linked)'],
    ]
    
    data_table = Table(data_specs, colWidths=[6*cm, 8*cm])
    data_table.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), LIGHT_BLUE),
        ('TEXTCOLOR', (0, 0), (-1, 0), PRIMARY_COLOR),
        ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
        ('FONTSIZE', (0, 0), (-1, -1), 10),
        ('ALIGN', (0, 0), (-1, -1), 'LEFT'),
        ('GRID', (0, 0), (-1, -1), 0.5, ACCENT_COLOR),
        ('VALIGN', (0, 0), (-1, -1), 'MIDDLE'),
        ('TOPPADDING', (0, 0), (-1, -1), 6),
        ('BOTTOMPADDING', (0, 0), (-1, -1), 6),
        ('LEFTPADDING', (0, 0), (-1, -1), 8),
    ]))
    story.append(Spacer(1, 0.3*cm))
    story.append(data_table)
    story.append(Spacer(1, 0.5*cm))
    
    # Variables analyzed
    story.append(Paragraph("<b>Variables Analyzed (Year-over-Year Growth):</b>", styles['CustomBody']))
    variables = [
        "GDP (Gross Domestic Product)",
        "Final Consumption Expenditure",
        "Gross Fixed Capital Formation (Investment)",
        "Exports of Goods and Services",
        "Imports of Goods and Services",
        "Population",
    ]
    for v in variables:
        story.append(Paragraph(f"• {v}", styles['BulletItem']))
    
    story.append(PageBreak())
    
    # === SUMMARY STATISTICS ===
    story.append(Paragraph("3. Summary Statistics", styles['SectionHeader']))
    
    summary_text = """
    The table below compares key macroeconomic indicators between World Cup winner 
    countries and non-winner countries. Statistical significance is denoted by 
    * (p<0.10), ** (p<0.05), *** (p<0.01).
    """
    story.append(Paragraph(summary_text.strip(), styles['CustomBody']))
    story.append(Spacer(1, 0.3*cm))
    
    # Summary statistics table (full sample)
    summary_data = [
        ['Variable', 'Winner', 'Non-winner', 't-test'],
        ['GDP (thousands of 2015 USD millions)', '2,277.67 (1,040.89)', '1,057.08 (2,493.32)', '17.48***'],
        ['Population (millions)', '68.23 (37.34)', '49.65 (149.16)', '4.49***'],
        ['GDP per capita', '35,020.24 (12,743.09)', '33,077.68 (19,305.46)', '3.52***'],
        ['Year-on-Year GDP growth (%)', '2.52 (3.29)', '3.39 (3.96)', '-7.49***'],
        ['Number of countries', '6', '42', ''],
        ['Number of observations', '1,315', '7,422', ''],
    ]
    
    summary_table = Table(summary_data, colWidths=[5.5*cm, 3.5*cm, 3.5*cm, 2*cm])
    summary_table.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), LIGHT_BLUE),
        ('TEXTCOLOR', (0, 0), (-1, 0), PRIMARY_COLOR),
        ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
        ('FONTSIZE', (0, 0), (-1, -1), 9),
        ('ALIGN', (0, 0), (0, -1), 'LEFT'),
        ('ALIGN', (1, 0), (-1, -1), 'CENTER'),
        ('GRID', (0, 0), (-1, -1), 0.5, ACCENT_COLOR),
        ('VALIGN', (0, 0), (-1, -1), 'MIDDLE'),
        ('TOPPADDING', (0, 0), (-1, -1), 5),
        ('BOTTOMPADDING', (0, 0), (-1, -1), 5),
        ('LEFTPADDING', (0, 0), (-1, -1), 6),
    ]))
    story.append(summary_table)
    story.append(Spacer(1, 0.5*cm))
    
    # Growth statistics
    story.append(Paragraph("<b>Growth Rate Summary (Full Sample):</b>", styles['CustomBody']))
    
    growth_data = [
        ['Variable', 'N', 'Mean', 'Std. Dev.', 'Min', 'Max'],
        ['GDP Growth (%)', '8,737', '3.26', '3.88', '-21.98', '28.15'],
        ['Consumption Growth (%)', '8,693', '3.14', '3.68', '-41.40', '41.68'],
        ['Investment Growth (%)', '8,693', '4.34', '12.32', '-74.77', '296.81'],
        ['Exports Growth (%)', '8,693', '6.05', '9.13', '-43.74', '77.06'],
        ['Imports Growth (%)', '8,693', '6.15', '11.07', '-63.73', '89.95'],
    ]
    
    growth_table = Table(growth_data, colWidths=[4*cm, 1.5*cm, 1.5*cm, 2*cm, 2*cm, 2*cm])
    growth_table.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), LIGHT_BLUE),
        ('TEXTCOLOR', (0, 0), (-1, 0), PRIMARY_COLOR),
        ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
        ('FONTSIZE', (0, 0), (-1, -1), 9),
        ('ALIGN', (0, 0), (0, -1), 'LEFT'),
        ('ALIGN', (1, 0), (-1, -1), 'CENTER'),
        ('GRID', (0, 0), (-1, -1), 0.5, ACCENT_COLOR),
        ('VALIGN', (0, 0), (-1, -1), 'MIDDLE'),
        ('TOPPADDING', (0, 0), (-1, -1), 5),
        ('BOTTOMPADDING', (0, 0), (-1, -1), 5),
    ]))
    story.append(growth_table)
    
    story.append(PageBreak())
    
    # === METHODOLOGY ===
    story.append(Paragraph("4. Methodology", styles['SectionHeader']))
    
    method_text = """
    The analysis employs two complementary econometric approaches to identify the causal 
    effect of World Cup victories on macroeconomic outcomes:
    """
    story.append(Paragraph(method_text.strip(), styles['CustomBody']))
    
    # Event Study
    story.append(Paragraph("<b>4.1 Event Study Design</b>", styles['CustomBody']))
    event_text = """
    The event study approach examines GDP growth dynamics around World Cup events, comparing 
    treated units (winners/hosts) to control units across a symmetric event window. This 
    method captures both pre-trends and post-treatment dynamics.
    """
    story.append(Paragraph(event_text.strip(), styles['CustomBody']))
    
    # SDID
    story.append(Paragraph("<b>4.2 Synthetic Difference-in-Differences (SDID)</b>", styles['CustomBody']))
    sdid_text = """
    SDID combines elements of synthetic control and difference-in-differences methods. 
    It constructs optimal weights for both control units and pre-treatment periods, 
    providing more robust treatment effect estimates than either method alone.
    """
    story.append(Paragraph(sdid_text.strip(), styles['CustomBody']))
    
    story.append(Spacer(1, 0.5*cm))
    
    # === WORLD CUP EVENTS ===
    story.append(Paragraph("5. World Cup Events Analyzed", styles['SectionHeader']))
    
    # Winners table
    story.append(Paragraph("<b>World Cup Winners in Sample:</b>", styles['CustomBody']))
    
    winners_data = [
        ['Year', 'Winner', 'Host'],
        ['1966', 'England (GBR)', 'England'],
        ['1974', 'West Germany (DEU)', 'West Germany'],
        ['1982', 'Italy (ITA)', 'Spain'],
        ['1990', 'West Germany (DEU)', 'Italy'],
        ['1998', 'France (FRA)', 'France'],
        ['2002', 'Brazil (BRA)', 'Japan/South Korea'],
        ['2006', 'Italy (ITA)', 'Germany'],
        ['2010', 'Spain (ESP)', 'South Africa'],
        ['2014', 'Germany (DEU)', 'Brazil'],
        ['2018', 'France (FRA)', 'Russia'],
    ]
    
    winners_table = Table(winners_data, colWidths=[2*cm, 5*cm, 5*cm])
    winners_table.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), LIGHT_BLUE),
        ('TEXTCOLOR', (0, 0), (-1, 0), PRIMARY_COLOR),
        ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
        ('FONTSIZE', (0, 0), (-1, -1), 10),
        ('ALIGN', (0, 0), (-1, -1), 'CENTER'),
        ('GRID', (0, 0), (-1, -1), 0.5, ACCENT_COLOR),
        ('VALIGN', (0, 0), (-1, -1), 'MIDDLE'),
        ('TOPPADDING', (0, 0), (-1, -1), 5),
        ('BOTTOMPADDING', (0, 0), (-1, -1), 5),
    ]))
    story.append(winners_table)
    
    story.append(PageBreak())
    
    # === OUTPUT GENERATED ===
    story.append(Paragraph("6. Analysis Outputs Generated", styles['SectionHeader']))
    
    output_text = """
    The project has produced extensive analytical outputs including visualizations and 
    statistical analyses:
    """
    story.append(Paragraph(output_text.strip(), styles['CustomBody']))
    
    # Event Plots
    story.append(Paragraph("<b>6.1 Event Study Plots (73 plots generated):</b>", styles['CustomBody']))
    event_plots = [
        "Individual winner country plots (GDP trends around World Cup victory)",
        "Summary overlay plots for all winners",
        "Host country event plots",
        "Feature-specific plots (consumption, investment, exports, imports)",
        "Average winner vs. host comparison plots",
    ]
    for p in event_plots:
        story.append(Paragraph(f"• {p}", styles['BulletItem']))
    
    story.append(Spacer(1, 0.3*cm))
    
    # SDID Results
    story.append(Paragraph("<b>6.2 SDID Analysis Results (5 outcome variables):</b>", styles['CustomBody']))
    sdid_outputs = [
        "SDID_GDP.png - GDP treatment effects",
        "SDID_Final_Consumption.png - Consumption treatment effects",
        "SDID_Gross_fixed_capital.png - Investment treatment effects",
        "SDID_Export.png - Export treatment effects",
        "SDID_Import.png - Import treatment effects",
    ]
    for s in sdid_outputs:
        story.append(Paragraph(f"• {s}", styles['BulletItem']))
    
    story.append(Spacer(1, 0.3*cm))
    
    # Argentina Extension
    story.append(Paragraph("<b>6.3 Argentina 2022 Extension:</b>", styles['CustomBody']))
    arg_text = """
    An extension analysis examines Argentina's 2022 World Cup victory, comparing 
    actual vs. counterfactual economic trajectories:
    """
    story.append(Paragraph(arg_text.strip(), styles['CustomBody']))
    
    arg_results = [
        ['Feature', 'Pre-WC Mean (%)', 'Post-WC Mean (%)', 'Change (pp)'],
        ['GDP', '0.43', '-1.58', '-2.02'],
        ['Consumption', '-0.42', '-0.92', '-0.50'],
        ['Investment', '3.40', '-9.32', '-12.71'],
        ['Exports', '1.33', '5.19', '+3.86'],
        ['Imports', '-0.21', '-3.95', '-3.74'],
    ]
    
    arg_table = Table(arg_results, colWidths=[3.5*cm, 3.5*cm, 3.5*cm, 3*cm])
    arg_table.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), LIGHT_BLUE),
        ('TEXTCOLOR', (0, 0), (-1, 0), PRIMARY_COLOR),
        ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
        ('FONTSIZE', (0, 0), (-1, -1), 9),
        ('ALIGN', (0, 0), (0, -1), 'LEFT'),
        ('ALIGN', (1, 0), (-1, -1), 'CENTER'),
        ('GRID', (0, 0), (-1, -1), 0.5, ACCENT_COLOR),
        ('VALIGN', (0, 0), (-1, -1), 'MIDDLE'),
        ('TOPPADDING', (0, 0), (-1, -1), 5),
        ('BOTTOMPADDING', (0, 0), (-1, -1), 5),
    ]))
    story.append(Spacer(1, 0.3*cm))
    story.append(arg_table)
    
    story.append(PageBreak())
    
    # === PROJECT FILES ===
    story.append(Paragraph("7. Project File Structure", styles['SectionHeader']))
    
    files_text = """
    Key files and directories in the project:
    """
    story.append(Paragraph(files_text.strip(), styles['CustomBody']))
    
    files_data = [
        ['File/Directory', 'Description'],
        ['oecd_manual_analysis.ipynb', 'Main data processing and analysis notebook'],
        ['wc_event_plots.ipynb', 'Event study visualization generation'],
        ['sdid_replication.ipynb', 'SDID analysis implementation'],
        ['summary_statistics.ipynb', 'Summary statistics and Table 1 replication'],
        ['argentina_wc2022_prediction.ipynb', 'Argentina 2022 extension analysis'],
        ['paper_replicate_sdid.R', 'R script for SDID replication'],
        ['paper_replicate_event_study.R', 'R script for event study replication'],
        ['Data/', 'Raw data files and processing scripts'],
        ['event descriptive plots/', 'Generated event study visualizations'],
        ['sdid results/', 'SDID analysis output plots'],
        ['argentina_2022_analysis/', 'Argentina extension outputs'],
    ]
    
    files_table = Table(files_data, colWidths=[5.5*cm, 8.5*cm])
    files_table.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), LIGHT_BLUE),
        ('TEXTCOLOR', (0, 0), (-1, 0), PRIMARY_COLOR),
        ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
        ('FONTSIZE', (0, 0), (-1, -1), 9),
        ('ALIGN', (0, 0), (-1, -1), 'LEFT'),
        ('GRID', (0, 0), (-1, -1), 0.5, ACCENT_COLOR),
        ('VALIGN', (0, 0), (-1, -1), 'MIDDLE'),
        ('TOPPADDING', (0, 0), (-1, -1), 5),
        ('BOTTOMPADDING', (0, 0), (-1, -1), 5),
        ('LEFTPADDING', (0, 0), (-1, -1), 6),
    ]))
    story.append(files_table)
    
    story.append(Spacer(1, 1*cm))
    
    # === REFERENCES ===
    story.append(Paragraph("8. Key Reference", styles['SectionHeader']))
    
    ref_text = """
    <b>Mello, L. (2024).</b> "A Kick for GDP? The Economic Effects of World Cup Success." 
    <i>Oxford Bulletin of Economics and Statistics.</i>
    """
    story.append(Paragraph(ref_text, styles['CustomBody']))
    
    # Build PDF
    doc.build(story)
    print(f"PDF created successfully: {output_path}")
    return output_path


if __name__ == '__main__':
    create_overview_pdf()
