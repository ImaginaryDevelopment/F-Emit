namespace FSharpWpfMvvmTemplate.ViewModel

open System
open System.Collections.ObjectModel
open System.Windows
open System.Windows.Data
open System.Windows.Input
open System.ComponentModel

type MainWindowViewModel() =
    inherit ViewModelBase()
    let frames = ["ExpenseItHome.xaml"; "InstanceExplorer.xaml"; "TableExplorer.xaml" ]
    let mutable frame = frames.[0]
    member x.SelectedFrame
        with get() = frame
        and set value = 
            frame <- value
            x.OnPropertyChanged "SelectedFrame"
    member x.SwitchFrameCommand =
        new RelayCommand ((fun _ -> true),
                fun _ -> x.SelectedFrame <-  if frame = frames.[0] then frames.[1] else frames.[0])